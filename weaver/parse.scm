(library (parse)

(export parse-file
) ; export

(import (chezscheme)
        (utf))

(define *read-buffer-size* 2)

; Line and column number.
; 1-based indexing.
(define-record-type source-pos
  (fields line column))

(define-record-type source-span
  (fields start-pos ; source-pos
          end-pos   ; source-pos
          ))

(define-record-type parse-state
  (fields          file-path                  ; optional path of the source file being parsed
                   input-port
                   read-buffer                 ; fixed-sized bytevector chunk read from input-port
          (mutable read-buffer-length)         ; count of bytes within the read-buffer byte vector
          (mutable read-buffer-index)          ; offset within read-buffer byte vector
                   codepoint-remainder         ; remainder bytes belonging to the previous read-buffer
          (mutable codepoint-remainder-length) ; count of bytes within codepoint-remainder byte vector
                   utf8-bytes-scratch-buffer   ; scratch buffer for utf8-bytes->char conversion 
                   uf-position                 ; user friendly source position
                   mode                        ; either 'raw or 'sexpr 
  ) ; fields
) ; parse-state

; Fills the read-buffer with the next chunk of the file
(define (read-next-chunk! parse-state)
  (let* ((input-port (parse-state-input-port parse-state))
         (read-buffer (parse-state-read-buffer parse-state))
         (result (get-bytevector-n! input-port read-buffer 0 *read-buffer-size*))
         (read-buffer-length (if (eof-object? result) 0 result)))
    (parse-state-read-buffer-length-set! parse-state read-buffer-length)
    (parse-state-read-buffer-index-set! parse-state 0)))

(define (%skip! parse-state) 
  (let* ((read-buffer                (parse-state-read-buffer                parse-state))
         (read-buffer-index          (parse-state-read-buffer-index          parse-state))
         (read-buffer-length         (parse-state-read-buffer-length         parse-state))
         (codepoint-remainder        (parse-state-codepoint-remainder        parse-state))
         (codepoint-remainder-length (parse-state-codepoint-remainder-length parse-state)))
    (let* ((leading-byte (if (= codepoint-remainder-length 0)
                           (bytevector-u8-ref read-buffer read-buffer-index)
                           (bytevector-u8-ref codepoint-remainder 0)))
           (sequence-length (utf8-sequence-length leading-byte)))
      (cond
        ; cross-boundary case: peek! already loaded the new chunk and saved remainder.
        ; just advance past the continuation bytes in the current buffer.
        ((> codepoint-remainder-length 0)
          (let ((bytes-from-cur-buf (- sequence-length codepoint-remainder-length)))
            (parse-state-read-buffer-index-set! 
              parse-state 
              (+ read-buffer-index bytes-from-cur-buf))))

        ; sequence spans across chunk boundary (no prior peek with remainder)
        ((> sequence-length (- read-buffer-length read-buffer-index))
          (let ((leftover (- sequence-length (- read-buffer-length read-buffer-index))))
            (read-next-chunk! parse-state)
            (parse-state-read-buffer-index-set! parse-state leftover)))
         
        ; normal case: just move the index forward
        (else 
          (parse-state-read-buffer-index-set! 
            parse-state 
            (+ read-buffer-index sequence-length))))

      ; clear codepoint-remainder buffer 
      (bytevector-fill! codepoint-remainder 0)
      (parse-state-codepoint-remainder-length-set! parse-state 0))))

(define (skip! parse-state)
  (let* ((read-buffer-index  (parse-state-read-buffer-index  parse-state))
         (read-buffer-length (parse-state-read-buffer-length parse-state)))
    (cond
      ; empty read-buffer
      ((= read-buffer-length 0) (error "skip!" "empty input"))

      ; end of read-buffer
      ((= (- read-buffer-length read-buffer-index) 0) 
        (begin 
          (read-next-chunk! parse-state)
          ; re-read length after the chunk load
          (if (= (parse-state-read-buffer-length parse-state) 0) 
            (error "skip!" "eof")
            (%skip! parse-state))))

      (else (%skip! parse-state)))))

(define (%peek! parse-state)
  (let* ((read-buffer                (parse-state-read-buffer                parse-state))
         (read-buffer-index          (parse-state-read-buffer-index          parse-state))
         (read-buffer-length         (parse-state-read-buffer-length         parse-state))
         (codepoint-remainder        (parse-state-codepoint-remainder        parse-state))
         (codepoint-remainder-length (parse-state-codepoint-remainder-length parse-state))
         (utf8-bytes-scratch-buffer  (parse-state-utf8-bytes-scratch-buffer  parse-state)))
    
    (let* ((leading-byte (if (= codepoint-remainder-length 0)
                           (bytevector-u8-ref read-buffer read-buffer-index)
                           (bytevector-u8-ref codepoint-remainder 0)))
           (sequence-length (utf8-sequence-length leading-byte))
           (bytes-remaining (- read-buffer-length read-buffer-index))
           (exceeds-read-buffer (> sequence-length bytes-remaining)))

      ; if the sequence spans a chunk boundary, save the partial bytes and load next chunk
      (when (and exceeds-read-buffer (= codepoint-remainder-length 0))
        ; only save remainder if we haven't already (i.e. first peek after boundary)
        (bytevector-copy! read-buffer read-buffer-index codepoint-remainder 0 bytes-remaining)
        
        (parse-state-codepoint-remainder-length-set! parse-state bytes-remaining)
        (set! codepoint-remainder-length bytes-remaining)

        (read-next-chunk! parse-state)
        (set! read-buffer-index (parse-state-read-buffer-index parse-state)))

      ; re-read after potential chunk load
      ; assemble full sequence into scratch buffer
      (bytevector-copy! 
        codepoint-remainder 0 
        utf8-bytes-scratch-buffer 0 
        codepoint-remainder-length)

      (bytevector-copy! 
        read-buffer read-buffer-index 
        utf8-bytes-scratch-buffer codepoint-remainder-length
        (- sequence-length codepoint-remainder-length))

      (let ((character (utf8-bytes->char utf8-bytes-scratch-buffer 0 sequence-length)))
        (bytevector-fill! utf8-bytes-scratch-buffer 0)
        ; NOTE: do NOT clear remainder or advance index here
        ; skip! is responsible for consuming the character and updating state.
        character))))

(define (peek! parse-state)
  (let* ((read-buffer-index  (parse-state-read-buffer-index  parse-state))
         (read-buffer-length (parse-state-read-buffer-length parse-state)))
    (cond
      ; empty read-buffer
      ((= read-buffer-length 0) (error "peek!" "empty input"))

      ; end of read-buffer
      ((= (- read-buffer-length read-buffer-index) 0) 
        (begin 
          (read-next-chunk! parse-state)
          ; re-read length after the chunk load
          (if (= (parse-state-read-buffer-length parse-state) 0) 
            (error "peek!" "eof")
            (%peek! parse-state))))

      (else (%peek! parse-state)))))

(define (step! parse-state)
  (begin 
    (if (= (parse-state-read-buffer-length parse-state) 0)
        (read-next-chunk! parse-state))

    (display (peek! parse-state))
    (skip! parse-state)
    (display (peek! parse-state))
    (skip! parse-state)
    (display (peek! parse-state))))

(define (run! parse-state)
  (step! parse-state))

(define (parse-file file-path)
  ; TODO: Specify UTF-8 transcoder(?)
  (let ((input-port (open-file-input-port file-path)))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let* ((read-buffer (make-bytevector *read-buffer-size* 0))
               (codepoint-remainder (make-bytevector 3 0))
               (utf8-bytes-scratch-buffer (make-bytevector 4 0))
               (uf-position (make-source-pos 1 1))
               (mode 'raw)
               (parse-state (make-parse-state file-path 
                                              input-port 
                                              read-buffer 
                                              0                         ; read-buffer-length
                                              0                         ; read-buffer-index
                                              codepoint-remainder
                                              0                         ; codepoint-remainder-length
                                              utf8-bytes-scratch-buffer
                                              uf-position 
                                              mode)))
              (run! parse-state)))
      (lambda () (close-port input-port)))))

) ; library (parse)
