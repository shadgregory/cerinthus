;; Cerinthus example using Apache POI
(require <cerinthus>)
(set-env! '((org.apache.poi poi 3.15)
	    (org.apache.poi poi-ooxml 3.15)
	    (org.apache.xmlbeans xmlbeans 2.6.0)))

(define doc (org.apache.poi.xwpf.usermodel.XWPFDocument))
(define par (invoke (as org.apache.poi.xwpf.usermodel.XWPFDocument doc) 'createParagraph))
(define run (invoke (as org.apache.poi.xwpf.usermodel.XWPFParagraph par) 'createRun))
(invoke (as org.apache.poi.xwpf.usermodel.XWPFRun run) 'setText "This is a paragraph!")
(define out (java.io.FileOutputStream (java.io.File "./foo.docx")))
(invoke (as org.apache.poi.xwpf.usermodel.XWPFDocument doc) 'write out)
(invoke (as java.io.FileOutputStream out) 'close)
