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
