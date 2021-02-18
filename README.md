# racket-infrastructure-userdb

Split out from [`pkg-index`](https://github.com/racket/pkg-index).

## Database format

It's a single directory, containing files, each file representing one
user account.

The *name* of each file is the *email address* of the user concerned.

The *contents* of each file is an association list containing at least
the user's email and the
[`bcrypt`](https://github.com/samth/bcrypt.rkt)ed form of the user's
password followed by a NUL byte. For example, a user's password file
might contain the the Racket term

```racket
((email "test@example.com")
 (password #"$2y$12$j4Qrs3Bhd2bEFGg03DvhWeAf4jbKzxG7G6PCPvrD2rYHB2bnucFOW\0"))
```

(for password "hi").

The association list may also contain a `properties` entry, which
contains *another* association list with application-specific
properties. **NB** The `properties` association list is internally
read into a hash table, meaning that duplicate keys will overwrite
each other and will be lost on user record updates.

## Suggested user registration and password reset flow

Here's the idea:

 - Account resets and new user registrations are done with the same
   flow.

 - A random code is generated and emailed to the user. Then, they log
   in with the code and a new password. If the code matches the one
   generated, the password is stored (overwriting the previous
   password, if any) and the login succeeds.
