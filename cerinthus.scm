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

(define displayln
  (lambda (str)
    (display str)
    (newline)))

;; string-append replacement
(define str
  (lambda (#!rest rest-args)
    (letrec ((create (lambda (args cat-str)
		       (cond
			((null? args) cat-str)
			((symbol? (car args))
			 (create (cdr args) (string-append cat-str (symbol->string (car args)))))
			((number? (car args))
			 (create (cdr args) (string-append cat-str (number->string (car args)))))
			(else
			 (create (cdr args) (string-append cat-str (car args))))))))
      (create rest-args ""))))

(define copy-url-to-file
  (lambda (url file)
    (try-catch
     (set! &<{&[file]} &<{&[url]})
     (ex java.io.FileNotFoundException
	 (displayln (str "Error! Could not download " url))
	 (exit)))))

;; create directory if necessary
(define check-directory
  (lambda (directory)
    (let ((base-dir (str (java.lang.System:getProperty "user.home") "/.cerinthus"))
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
    (displayln (str "Adding " filepath))
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
    (let* ((file (File (file-string:toString)))
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
		  (letrec ((download
		    (lambda (i)
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
				    (set-env! `((,groupid ,artifactid ,version)) #t)
				    (if (not (= i 0))
					(download (- i 1)))))))))))
		    (download (- (invoke (as NodeList depList) 'getLength) 1)))))))

(define get-latest-version
  (lambda (group-id artifact-id)
    (let ((maven-search-url (str "https://search.maven.org/solrsearch/select?q=g:%22"
					   group-id
					   "%22+AND+a:%22"
					   artifact-id
					   "\"&wt=xml"))
	  (local-xml-file (str
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
				  ((string=? "latestVersion" ((as Element node):getAttribute "name"))
				   (node:getTextContent))
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
	     (maven-jar-url (str "https://repo1.maven.org/maven2/"
				       (regex-replace* "\\." group-id "/")
				       "/" artifact-id
				       "/" version "/"
				       artifact-id "-" version ".jar"))
	     (maven-pom-url (str "https://repo1.maven.org/maven2/"
				       (regex-replace* "\\." group-id "/")
				       "/" artifact-id
				       "/" version "/"
				       artifact-id "-" version ".pom"))
	     (local-jar-file (str (java.lang.System:getProperty "user.home")
					"/.cerinthus/"
					(regex-replace* "\\." group-id "/") "/"
					artifact-id "-"
					version ".jar"))
	     (local-pom-file (str (java.lang.System:getProperty "user.home")
					"/.cerinthus/"
					(regex-replace* "\\." group-id "/") "/"
					artifact-id "-"
					version ".pom")))
	(cond
	 ((null? dependencies) '())
	 ((or (not (file-exists? local-jar-file))
	      (and (not (file-exists? local-pom-file))(not no-pom?)))
	  (begin
	    (check-directory (str (regex-replace* "\\." group-id "/") "/"))
	    (displayln (str "Downloading : " maven-jar-url))
	    (copy-url-to-file maven-jar-url local-jar-file)
	    (if (eq? no-pom? #f)
		(begin
		  (displayln (str "Downloading : " maven-pom-url))
		  (copy-url-to-file maven-pom-url local-pom-file)
		  (get-dependencies local-pom-file)))
	      (set-jar! local-jar-file)
	      (set-env! (cdr dependencies))))
	 (else
	  (set-jar! local-jar-file)
	  (if (eq? no-pom? #f)
	      (get-dependencies local-pom-file))
	  (set-env! (cdr dependencies)))))))))
