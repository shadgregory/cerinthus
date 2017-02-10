# Cerinthus
Add java libraries to your kawa project simply and easily using Cerinthus

## Usage

Add cerinthus.scm to the root directory of your kawa project. The following example code downloads the apache poi library and adds it to your runtime.

```scm
(require <cerinthus>)
(set-env! '((org.apache.poi poi 3.15)
	    (org.apache.poi poi-ooxml 3.15)
	    (org.apache.xmlbeans xmlbeans 2.6.0)))
```

Version numbers are optional; Cerinthus will simply download the latest version.

```scm
(require <cerinthus>)
(set-env! '((org.apache.poi poi)
	    (org.apache.poi poi-ooxml)
	    (org.apache.xmlbeans xmlbeans)))
```

Kawa conveniently supports the unix-style she-bang for scripting. Cerinthus makes writing kawa scripts even easier.

```scm
#!/usr/local/bin/kawa

;;example kawa script that also helps us learn more about Cerinthus
(require <cerinthus>)
(set-env! '((commons-io commons-io 2.4)))
(define-alias FileUtils org.apache.commons.io.FileUtils)
(define-alias URL java.net.URL)
(define-alias File java.io.File)

(define url (URL "http://www.earlychristianwritings.com/text/irenaeus-book1.html"))
(FileUtils:copyURLToFile url (File "./irenaeus-book1.html"))
```
