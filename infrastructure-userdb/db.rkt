#lang racket/base

(provide list-users
         user-exists?
         lookup-user
         save-user!)

(module+ implementation
  (provide build-path^
           user-path
           lookup-user-error))

(require racket/match)
(require (only-in racket/port port->bytes))
(require (only-in racket/file make-directory* call-with-atomic-output-file))
(require (prefix-in bcrypt- bcrypt))
(require "types.rkt")

;; This f o f^-1 is applied because it throws an error if file is not
;; a single path element. This causes things like "../../etc/passwd"
;; to throw errors and thus be protected.
(define (build-path^ base file)
  (build-path base (path-element->string (string->path-element file))))

(define (alist->hash xs)
  (for/hash [(entry (in-list xs))]
    (match-define (list key value) entry)
    (values key value)))

(define ((lookup-user-error email) code . details)
  (define (complain fmt . args)
    (define msg (apply format fmt args))
    (fprintf (current-error-port) "~a\n" msg)
    (log-error "~a" msg)
    #f)
  (match (cons code details)
    [`(exception ,e)
     (complain "Exception looking up user record for ~v: ~e" email e)]
    [`(missing-key ,key)
     (complain "User record for ~v is missing key ~a" email key)]
    [`(email-address-mismatch ,_email ,other-email)
     (complain "User record for ~v contains incorrect email ~v" email other-email)]
    [other
     (complain "Unknown error retrieving user record for ~v: ~v" email other)]))

(define (list-users config)
  (map path->string (directory-list (userdb-config-directory config))))

(define (user-exists? config email)
  (and (lookup-user config
                    email
                    (lambda (code . details)
                      (if (eq? code 'exception)
                          #f
                          (apply (lookup-user-error email) code details))))
       #t))

(define (user-path config email)
  (build-path^ (userdb-config-directory config) email))

(define (lookup-user config email [on-error (lookup-user-error email)])
  (with-handlers [(exn:fail? (lambda (e) (on-error 'exception e)))]
    (define raw
      (with-input-from-file (user-path config email)
        (lambda ()
          (match (peek-char)
            [#\$
             ;; Old-style, password-only user database entry.
             (define bs (port->bytes))
             `((email ,email) (password ,bs))]
            [_
             ;; New-style alist, presumably.
             (read)]))))
    (define (get key ks [kf on-error])
      (match (assq key raw)
        [#f (kf 'missing-key key)]
        [(list _ value) (ks value)]))
    (get 'email
         (lambda (email2)
           (if (not (equal? email email2))
               (on-error 'email-address-mismatch email email2)
               (get 'password
                    (lambda (password-hash)
                      (get 'properties
                           (lambda (properties)
                             (user-info email password-hash (alist->hash properties)))
                           (lambda _missing-key-error-details
                             (user-info email password-hash (hash)))))))))))

(define (save-user! config info)
  (when (not (userdb-config-write-permitted? config))
    (error 'save-user! "Write not permitted to userdb ~v" config))
  (match-define (user-info email password-hash properties) info)
  (make-directory* (userdb-config-directory config))
  (call-with-atomic-output-file
   (user-path config email)
   (lambda (port _tmp-path)
     (write `((email ,email)
              (password ,password-hash)
              (properties ,(for/list [((k v) (in-hash properties))]
                             (list k v))))
            port))))
