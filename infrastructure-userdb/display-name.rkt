#lang racket/base

;; This module implements a “display name” system in response to the spam
;; problem reported in <https://github.com/racket/racket-pkg-website/issues/77>.
;; It lives here because both `racket-pkg-website` and `pkg-index` need it,
;; but neither of them depends on the other.
;;
;; Email addresses are obfuscated by combining the `local-part` of the email
;; address (i.e. the part to the left of the `@`, per RFC 5322 § 3.4.1) with
;; the first seven hexadecimal digits of the SHA-256 hash of the full
;; address. Thus, for example, the email address:
;;
;;     philip@philipmcgrath.com
;;
;; produces the obfuscated display name:
;;
;;     philipλ9411372
;;
;; Compared to alternative approaches, this design seems to have some nice
;; properties:
;;
;;  1. We can continue to use email addresses as the “primary key” to identify
;;     users, rather than having to create and maintain a registry for
;;     usernames in a global namespace.
;;
;;  2. We use the identifier all users already have chosen. All users get
;;     meaningful, yet obfuscated, display names by default, with no action
;;     needed. We have no new data fields to store or validate.
;;
;;  3. There is no ambiguity between email addresses and obfuscated display
;;     names: the former always contain `@`; the later never do.
;;
;;  4. Anyone who knows an email address can compute the corresponding
;;     obfuscated display name and thus can search for packages associated
;;     with it. In some contexts one would want to use a UUID or a salted hash
;;     to avoid making that information discoverable. Here, though, our goal
;;     is to protect package authors from being spammed, not to conceal the
;;     authorship of Racket packages.
;;
;; All of this module’s exports are pure functions. This module doesn’t depend
;; on any other parts of this package, but it provides sufficient primitives
;; to implement an obfuscation opt-out mechanism elsewhere using user properties.
;;

(require racket/match
         racket/contract
         (only-in file/sha1 bytes->hex-string)
         (only-in xml xexpr/c))

(provide display-name?
         (contract-out
          [email->display-name
           (-> string? display-name?)]
          [display-name-email
           (-> display-name? string?)]
          [display-name-obfuscated-email
           (-> display-name? string?)]
          [display-name-local-part
           (-> display-name? string?)]
          [display-name-short-hash
           (-> display-name? string?)]
          [display-name-tags
           (-> display-name? (listof symbol?))]
          [display-name-plain-tag
           (-> display-name? symbol?)]
          [display-name-obfuscated-tag
           (-> display-name? symbol?)]))

(module+ test
  (require (submod ".."))) ; enforce contracts

(struct display-name (email local-part short-hash plain-tag obfuscated-tag)
  #:guard (struct-guard/c string? string? string? symbol? symbol?)
  #:transparent)

(define email->display-name
  (let ([plain-tag-memo (make-weak-hasheq)]
        [display-name-memo (make-ephemeron-hasheq)])
    ;; indirection because equal?-based hashes aren't thread-safe
    (define (email->display-name email)
      (define plain-tag
        (hash-ref! plain-tag-memo
                   email
                   (λ ()
                     (email->plain-tag email))))
      (hash-ref! display-name-memo
                 plain-tag
                 (λ ()
                   (make-display-name email plain-tag))))
    email->display-name))

(define (email->plain-tag email)
  (string->symbol (string-append "author:" email)))

(define (make-display-name email [plain-tag (email->plain-tag email)])
  (match-define (list local-part)
    (regexp-match #rx"^[^@]*" email))
  (define short-hash
    (substring (bytes->hex-string
                (sha256-bytes (open-input-string email)))
               0
               7)) ; on the very scientific basis that this is what Git uses
  (display-name
   (string->immutable-string email)
   (string->immutable-string local-part)
   (string->immutable-string short-hash)
   plain-tag
   (string->symbol (string-append "author:" local-part "λ" short-hash))))

(module+ test
  (for ([expected (list (display-name "philip@philipmcgrath.com"
                                      "philip"
                                      "9411372"
                                      'author:philip@philipmcgrath.com
                                      'author:philipλ9411372)
                        (display-name "samth@racket-lang.org"
                                      "samth"
                                      "5dd761c"
                                      'author:samth@racket-lang.org
                                      'author:samthλ5dd761c))])
    (define actual
      (email->display-name (display-name-email expected)))
    (unless (equal? actual expected)
      (raise-arguments-error 'email->display-name "unexpected result"
                             "result" actual
                             "expected" expected))
    ;; check no contract violations
    (for-each (λ (f)
                (f actual))
              (list display-name-email
                    display-name-obfuscated-email
                    display-name-local-part
                    display-name-short-hash
                    display-name-tags
                    display-name-plain-tag
                    display-name-obfuscated-tag))))

(define (display-name-tags dn)
  (list (display-name-plain-tag dn)
        (display-name-obfuscated-tag dn)))

(define (display-name-obfuscated-email dn)
  (match-define (struct* display-name ([local-part local-part]
                                       [short-hash short-hash]))
    dn)
  (string-append local-part "λ" short-hash))
