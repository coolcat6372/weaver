(library (parse)

(export parse-file)

(import (chezscheme)
        (utf))

(define-record-type pos
  (fields file-path
          line
          column))

(define (pos-next pos char)
  (cond
    ((char=? char #\newline)
     (make-pos (pos-file-path pos) (+ 1 (pos-line pos)) 1))
    (else
     (make-pos (pos-file-path pos) 
               (pos-line pos) 
               (+ 1 (pos-column pos))))))

(define-record-type span
  (fields beginning
          end))

(define-record-type lexeme
  (fields location ; optional, either pos or span
          kind
          content))

(define-record-type parse-error
  (fields location ; optional, either pos or span
          message))

(define (p-error-here parse-state message)
  (make-parse-error (parse-state-uf-position parse-state) message))

(define-record-type parse-state
  (fields          file-path    ; optional path of the source file being parsed
                   content-bv   ; entire file content as a bytevector
          (mutable offset)      ; current offset within content-bv
          (mutable uf-position) ; user friendly source position
          (mutable mode)))

(define (p-eof? p)
  (>= (parse-state-offset p) (bytevector-length (parse-state-content-bv p))))

(define (p-spanned p body f)
  (let ((start (parse-state-uf-position p))
        (a     (body)))
    (f (make-span start (parse-state-uf-position p)) a)))

(define (skip! p)
  (if (p-eof? p) (raise (p-error-here p "unexpected end of file")))
  (let* ((content-bv      (parse-state-content-bv p))
         (offset          (parse-state-offset p))
         (uf-position     (parse-state-uf-position p))
         (leading-byte    (bytevector-u8-ref content-bv offset))
         (sequence-length (utf8-sequence-length leading-byte))
         (character       (utf8-bytes->char content-bv offset sequence-length)))
    (parse-state-offset-set! p (+ offset sequence-length))
    (parse-state-uf-position-set! p (pos-next uf-position character))))

(define (peek! p)
  (if (p-eof? p) (raise (p-error-here p "unexpected end of file")))
  (let* ((content-bv      (parse-state-content-bv p))
         (offset          (parse-state-offset p))
         (leading-byte    (bytevector-u8-ref content-bv offset))
         (sequence-length (utf8-sequence-length leading-byte)))
    (utf8-bytes->char content-bv offset sequence-length)))

(define (peek-offset! p n)
  (if (p-eof? p) (raise (p-error-here p "unexpected end of file")))
  (let* ((content-bv      (parse-state-content-bv p))
         (offset          (parse-state-offset p))
         (offset-n        (+ offset (utf8-cumulative-sequence-length content-bv offset n)))
         (sequence-length (utf8-sequence-length (bytevector-u8-ref content-bv offset-n))))
    (utf8-bytes->char content-bv offset-n sequence-length)))

(define (consume! p)
  (let ((character (peek! p)))
    (skip! p)
    character))

(define (p-until-eof p thunk)
  (unless (p-eof? p)
    (when (thunk) (p-until-eof p thunk))))

(define (parse-raw! p)
  (let ((out #f))
    (p-spanned p 
      (lambda () 
        (p-until-eof p 
          (lambda () 
            (cond
              ((and (eqv? (peek! p) #\\) 
                    (not (eqv? (peek-offset! p 1) #\\))) 
                #f)
              (else  
                (begin 
                  (if (eqv? out #f) 
                    (set! out (open-output-string)))
                  
                  ; TODO: properly handle escape sequences
                  (if (eqv? (peek! p) #\\)
                    (skip! p))

                  (write-char (consume! p) out)
                  #t))))))
      (lambda (span _) 
        (if (eqv? out #f)
          #f
          (make-lexeme 'raw span (get-output-string out)))))))

(define (parse-escaped-sexpr! p)
  (if (or (p-eof? p) (not (eqv? (peek! p) #\\)))
    #f
    (let ((next (peek-offset! p 1)))
      (if (not (eqv? next #\())
        #f
        (p-spanned p
          (lambda ()
            (skip! p) ; consume `\`
            (skip! p) ; consume `(`
            (let ((out  (open-output-string))
                  (depth 1))
              (p-until-eof p
                (lambda ()
                  (let ((char (consume! p)))
                    (cond
                      ((eqv? char #\()
                        (set! depth (+ depth 1))
                        (write-char char out)
                        #t)
                      ((eqv? char #\))
                        (set! depth (- depth 1))
                        (if (= depth 0)
                          #f ; done
                          (begin (write-char char out) #t)))
                      (else
                        (write-char char out)
                        #t)))))
              (with-input-from-string
                (string-append "(" (get-output-string out) ")")
                read)))
          (lambda (span datum)
            (make-lexeme 'sexpr span (list 'quote datum))))))))

(define (step! p)
  (if (p-eof? p)
    #f
    (or (parse-escaped-sexpr! p)
        (parse-raw! p))))

(define (run! p)
  (display (step! p))
  (display (step! p))
  (display (step! p)))

(define (parse-file file-path)
  (let* ((input-port (open-file-input-port file-path))
         (content-bv (get-bytevector-all input-port))
         (parse-state 
           (make-parse-state file-path 
                             content-bv 
                             0
                             (make-pos file-path 1 1) 
                             'raw)))
    (close-port input-port)
    (run! parse-state)))

) ; library (parse)