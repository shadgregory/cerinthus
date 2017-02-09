(module-name cerinthus)
(module-export set-env!)
(import (kawa regex))
(define-alias File java.io.File)
(define-alias URI java.net.URI)
(define-alias URL java.net.URL)
(define-alias URLClassLoader java.net.URLClassLoader)
(define-alias Element org.w3c.dom.Element)
(define-alias NodeList org.w3c.dom.NodeList)
(define-alias Document org.w3c.dom.Document)

;; string-append replacement macro
(define-syntax str
  (syntax-rules ()
    ((_) "")
    ((_ string1 . rest)
     (cond
      ((null? string1)
       (string-append "" (str . rest)))
      ((boolean? string1)
       (cond
	((not string1)
	 (string-append "FALSE" (str . rest)))
	(else
	 (string-append "TRUE" (str . rest)))))
      ((symbol? string1)
       (string-append (symbol->string string1) (str . rest)))
      ((number? string1)
       (string-append (number->string string1) (str  . rest)))
      (else
       (string-append string1 (str . rest)))))))

;; create directory if necessary
(define check-directory
  (lambda (directory)
    (let ((base-dir (str (java.lang.System:getProperty "user.home") "/.kawa-env"))
	  (directories (regex-split "/" directory)))
      (if (not (file-exists? base-dir))
	  (create-directory base-dir))
      (letrec ((create (lambda (dirs current)
			 (cond
			  ((null? dirs) '())
			  (else
			   (begin
			     (create-directory (str current "/" (car dirs)))
			     (create (cdr dirs) (str current "/" (car dirs)))))))))
	(create directories base-dir)))))

(define set-jar!
  (lambda (filepath)
    (display (str "Adding " filepath))
    (newline)
    (let* ((file (File (filepath:toString)))
	   (uri (invoke (as File file) 'toURI))
	   (url (invoke (as URI uri) 'toURL))
	   (system-classloader (java.lang.ClassLoader:getSystemClassLoader))
	   (classloader-class URLClassLoader:class)
	   (url-class URL:class)
	   (method (invoke (as java.lang.Class classloader-class) 'getDeclaredMethod "addURL" (java.lang.Class[] url-class))))
      (method:setAccessible #t)
      (method:invoke system-classloader (object[] url)))))

;; create org.w3c.dom.Document object from file path string
(define create-doc
  (lambda (file-string)
    (let* (
	   (file (File (file-string:toString)))
	   (db-factory (javax.xml.parsers.DocumentBuilderFactory:newInstance))
	   (db-builder (invoke (as javax.xml.parsers.DocumentBuilderFactory db-factory) 'newDocumentBuilder))
	   (doc (invoke (as javax.xml.parsers.DocumentBuilder db-builder) 'parse (as File file)))
	   (doc-element (invoke (as org.w3c.dom.Document doc) 'getDocumentElement)))
      (doc-element:normalize)
      doc)))

(define get-dependencies
  (lambda (pom-file)
    (let ((doc (create-doc pom-file)))
      (let ((depList (doc:getElementsByTagName "dependency")))
	(do ((i (- (invoke (as NodeList depList) 'getLength) 1) (- i 1)))
	    ((< i 0))
	  (let* ((node (depList:item i))
		 (version-list (invoke (as Element node) 'getElementsByTagName "version"))
		 (version-item (invoke (as NodeList version-list) 'item 0))
		 (groupid-list (invoke (as Element node) 'getElementsByTagName "groupId"))
		 (groupid-item (invoke (as NodeList groupid-list) 'item 0))
		 (artifactid-list (invoke (as Element node) 'getElementsByTagName "artifactId"))
		 (artifactid-item (invoke (as NodeList artifactid-list) 'item 0)))
	    (if (not (eq? #!null version-item))
		(let ((version (version-item:getTextContent))
		      (artifactid (artifactid-item:getTextContent))
		      (groupid (groupid-item:getTextContent)))
		  (if (regex-match #/^[0-9.]/ version)
		      (begin
			(set-env! `((,groupid ,artifactid ,version)) #t)))))))))))

(define get-latest-version
  (lambda (group-id artifact-id)
    (let ((maven-search-url (string-append "https://search.maven.org/solrsearch/select?q=g:%22"
					   group-id
					   "%22+AND+a:%22"
					   artifact-id
					   "\"&wt=xml"))
	  (local-xml-file (string-append
			   (java.lang.System:getProperty "java.io.tmpdir")
			   (java.lang.System:getProperty "file.separator")
			   artifact-id ".xml")))
      (set! &<{&[local-xml-file]} &<{&[maven-search-url]})
      ;;gotta extract this
      (let ((doc (create-doc local-xml-file)))
	(let ((str-list (doc:getElementsByTagName "str")))
	  (letrec ((find-ver (lambda (cnt)
			       (let ((node (str-list:item cnt)))
				 (cond
				  ((= cnt (- (str-list:getLength) 1)) "")
				  ((string=? "latestVersion" ((as org.w3c.dom.Element node):getAttribute "name")) (node:getTextContent))
				  (else
				   (find-ver (+ cnt 1))))))))
	    (find-ver 0)))))))

;; dependencies list of lists -> group-id artifact-id version
(define set-env!
  (lambda (dependencies #!optional no-pom?)
    (cond
     ((null? dependencies) '())
     (else
      (let* ((group-id (str (car (car dependencies))))
	     (artifact-id (str (car (cdr (car dependencies)))))
	     (version
	      (if (null? (cdr (cdr (car dependencies))))
		  (get-latest-version group-id artifact-id)
		  (str (car (cdr (cdr (car dependencies)))))))
	     (maven-jar-url (string-append "https://repo1.maven.org/maven2/"
				       (regex-replace* "\\." group-id "/")
				       "/" artifact-id
				       "/" version "/"
				       artifact-id "-" version ".jar"))
	     (maven-pom-url (string-append "https://repo1.maven.org/maven2/"
				       (regex-replace* "\\." group-id "/")
				       "/" artifact-id
				       "/" version "/"
				       artifact-id "-" version ".pom"))
	     (local-jar-file (string-append (java.lang.System:getProperty "user.home")
					"/.kawa-env/"
					(regex-replace* "\\." group-id "/") "/"
					artifact-id "-"
					version ".jar"))
	     (local-pom-file (string-append (java.lang.System:getProperty "user.home")
					"/.kawa-env/"
					(regex-replace* "\\." group-id "/") "/"
					artifact-id "-"
					version ".pom")))
	(cond
	 ((null? dependencies) '())
	 ((or (not (file-exists? local-jar-file))
	      (and (not (file-exists? local-pom-file))(not no-pom?)))
	  (begin
	    (check-directory (string-append (regex-replace* "\\." group-id "/") "/"))
	    (display (str "Downloading : " maven-jar-url))
	    (newline)
	    (set! &<{&[local-jar-file]} &<{&[maven-jar-url]})
	    (if (eq? no-pom? #f)
		(begin
		  (display (str "Downloading : " maven-pom-url))
		  (newline)
		  (set! &<{&[local-pom-file]} &<{&[maven-pom-url]})
		  (get-dependencies local-pom-file)))
	      (set-jar! local-jar-file)
	      (set-env! (cdr dependencies))))
	 (else
	  (set-jar! local-jar-file)
	  (if (eq? no-pom? #f)
	      (get-dependencies local-pom-file))
	  (set-env! (cdr dependencies)))))))))
