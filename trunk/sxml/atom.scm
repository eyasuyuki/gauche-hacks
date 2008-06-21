
(use rfc.uri)
(use rfc.http)
(use util.match)
(use gauche.charconv)
(use sxml.ssax)
(use sxml.sxpath)

(define-constant NAMESPACES
  '((rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rss . "http://purl.org/rss/1.0/")
    (dc  . "http://purl.org/dc/elements/1.1/")
    (atom . "http://www.w3.org/2005/Atom")))

(define (body-encoding body)
  (and-let* ((body   (string-complete->incomplete body))
             (before (string-scan body #*"?>" 'before))
             (enc    (string-scan before #*"encoding='" 'after))
             (enc2   (string-scan enc #*"'" 'before)))
    enc2))

(define (fetch-atom uri)
  (receive (scm usr host port path query frag)
      (uri-parse uri)
    (receive (status headers body)
        (http-get host path)
      (and-let* (((equal? status "200"))
                 ((string? body))
                 (encoding (body-encoding body)))
	(ssax:xml->sxml
	 (wrap-with-input-conversion (open-input-string body) encoding)
	 NAMESPACES)))))

(define (extract-atom atom)
  (match atom
	 [(top ns pi (feed . rest)) rest]))
