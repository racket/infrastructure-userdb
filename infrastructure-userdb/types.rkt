#lang racket/base

(provide (struct-out userdb-config)
         (struct-out user-info)
         make-user
         user-password-set
         user-password-correct?
         user-property
         user-property-set)

(require (prefix-in bcrypt- bcrypt))

(struct userdb-config (directory ;; path - directory where user records are kept
                       write-permitted? ;; boolean - #t to allow creating/saving/updating user records
                       ) #:prefab)

(struct user-info (email ;; string
                   password-hash ;; bcrypted byte string
                   properties ;; hash from value to value
                   ) #:prefab)

(define (hash-pw password-string)
  (bcrypt-encode (string->bytes/utf-8 password-string)))

(define (make-user email password-string)
  (user-info email (hash-pw password-string) (hash)))

(define (user-password-set info password-string)
  (struct-copy user-info info [password-hash (hash-pw password-string)]))

(define (user-password-correct? info given-password)
  ;; Do not trim or otherwise modify given-password: the user may have
  ;; spaces at the beginning or end of their password!
  (and (user-info? info)
       (bcrypt-check (user-info-password-hash info) (string->bytes/utf-8 given-password))))

(define (user-property info key [on-error #f])
  (hash-ref (user-info-properties info) key on-error))

(define (user-property-set info key value)
  (struct-copy user-info info [properties (hash-set (user-info-properties info) key value)]))
