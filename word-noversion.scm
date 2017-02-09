;; Cerinthus example using Apache POI without the version numbers
(require <cerinthus>)
(set-env! '((org.apache.poi poi)
	    (org.apache.poi poi-ooxml)
	    (org.apache.xmlbeans xmlbeans)))

(define doc (org.apache.poi.xwpf.usermodel.XWPFDocument))
(define par (invoke (as org.apache.poi.xwpf.usermodel.XWPFDocument doc) 'createParagraph))
(define run (invoke (as org.apache.poi.xwpf.usermodel.XWPFParagraph par) 'createRun))
(invoke (as org.apache.poi.xwpf.usermodel.XWPFRun run) 'setText "This is a paragraph!")
(define out (java.io.FileOutputStream (java.io.File "./foo.docx")))
(invoke (as org.apache.poi.xwpf.usermodel.XWPFDocument doc) 'write out)
(invoke (as java.io.FileOutputStream out) 'close)
