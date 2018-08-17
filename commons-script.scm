#!/usr/local/bin/kawa -f

;; an example kawa script that also helps us learn more about Cerinthus
(require <cerinthus>)
(set-env! '((commons-io commons-io 2.6)))
(define-alias FileUtils org.apache.commons.io.FileUtils)
(define cl (invoke-static java.lang.ClassLoader 'getSystemClassLoader))
(define urls (invoke (as java.net.URLClassLoader cl) 'getURLs))
(define-alias URL java.net.URL)
(define-alias File java.io.File)

(define url (URL "http://www.earlychristianwritings.com/text/irenaeus-book1.html"))
(FileUtils:copyURLToFile url (File "./irenaeus-book1.html"))
