(library (utf)

(export utf8-sequence-length
        utf8-bytes->codepoint
        utf8-bytes->char)

(import (chezscheme))

; Returns the expected byte length of a UTF-8 sequence from its leading byte.
(define (utf8-sequence-length leading-byte)
  (cond
    ((= (bitwise-and leading-byte #b10000000) #b00000000) 1) ; 0xxxxxxx
    ((= (bitwise-and leading-byte #b11100000) #b11000000) 2) ; 110xxxxx
    ((= (bitwise-and leading-byte #b11110000) #b11100000) 3) ; 1110xxxx
    ((= (bitwise-and leading-byte #b11111000) #b11110000) 4) ; 11110xxx
    (else (error "utf8-sequence-length" "invalid utf-8 leading byte"))))

; Decodes a UTF-8 sequence from a bytevector at a given offset into a codepoint integer.
; `bv`     - bytevector containing the sequence
; `offset` - starting index of the sequence in the bytevector
; `len`    - number of bytes in the sequence (from utf8-sequence-length)
(define (utf8-bytes->codepoint bv offset len)
  (case len
    ((1)
     (bytevector-u8-ref bv offset))
    ((2)
     (bitwise-ior
       (bitwise-arithmetic-shift-left (bitwise-and (bytevector-u8-ref bv offset)       #b00011111) 6)
       (bitwise-and (bytevector-u8-ref bv (+ offset 1)) #b00111111)))
    ((3)
     (bitwise-ior
       (bitwise-arithmetic-shift-left (bitwise-and (bytevector-u8-ref bv offset)       #b00001111) 12)
       (bitwise-arithmetic-shift-left (bitwise-and (bytevector-u8-ref bv (+ offset 1)) #b00111111)  6)
       (bitwise-and (bytevector-u8-ref bv (+ offset 2)) #b00111111)))
    ((4)
     (bitwise-ior
       (bitwise-arithmetic-shift-left (bitwise-and (bytevector-u8-ref bv offset)       #b00000111) 18)
       (bitwise-arithmetic-shift-left (bitwise-and (bytevector-u8-ref bv (+ offset 1)) #b00111111) 12)
       (bitwise-arithmetic-shift-left (bitwise-and (bytevector-u8-ref bv (+ offset 2)) #b00111111)  6)
       (bitwise-and (bytevector-u8-ref bv (+ offset 3)) #b00111111)))
    (else (error "utf8-bytes->codepoint" "invalid sequence length" len))))

; Decodes a UTF-8 sequence from a bytevector at a given offset into a Scheme char.
(define (utf8-bytes->char bv offset len)
  (integer->char (utf8-bytes->codepoint bv offset len)))

) ; library (utf)