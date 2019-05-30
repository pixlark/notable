#lang racket

(define (concat x . xs)
  (string-join (cons x xs)))

(define (simple-definition name definition)
  (format "~a \\(\\rightarrow\\) ~a" name definition))

(define (transform-and-paren equation)
  (let ([s (transform-equation equation)])
    (match equation
      [(cons symbol args)
       #:when (symbol? symbol)
       (format "(~a)" s)]
      [else s])))

(define (transform-equation equation)
  (match equation
    [symbol #:when (symbol? symbol)
            (let ([s (symbol->string symbol)])
              (if (> (string-length s) 1)
                  (format "\\operatorname{~a}" s)
                  s))]
    [(cons symbol args)
     #:when (and (symbol? symbol)
                 (member symbol '(= + - * ** //)))
     (string-join (map transform-and-paren args)
                  (match symbol
                    ['= " = "]
                    ['+ " + "]
                    ['- " - "]
                    ['* ""]
                    ['** " * "]
                    ['// " \\div "]))]
    [(cons '/ args)
     #:when (= (length args) 2)
     (format "\\frac{~a}{~a}"
             (transform-equation (first args))
             (transform-equation (second args)))]
    [(cons symbol args)
     #:when (symbol? symbol)
     (format "~a(~a)" (transform-equation symbol)
             (string-join (map transform-equation args)
                          ", "))]
    [else (error "Syntax error")]))

(define (function-definition name definition)
  (match definition
    [(list 'equation equation)
     (simple-definition
      name (concat "\\("
                   (transform-equation equation)
                   "\\)"))]
    [else (error "Syntax error")]))

(define (transform-decl decl)
  (match decl
    [(list 'section name body ...)
     #:when (string? name)
     (string-join
      (cons (format "\\section{~a}" name)
            (map transform-decl body))
      "\n")]
    [(list 'define name definition)
     #:when (and (string? name)
                 (string? definition))
     (simple-definition name definition)]
    [(list 'define name definition)
     #:when (and (string? name)
                 (list? definition))
     (function-definition name definition)]
    [else (error "Syntax error")]))

(define (transform-decls decls)
  (string-join
   `("\\documentclass{article}"
     "\\usepackage{amsmath}"
     "\\begin{document}"
     ,(string-join (map transform-decl decls) "\n\n")
     "\\end{document}")
   "\n"))

(define-syntax-rule (module-begin body ...)
  (#%module-begin
   (let ([output (transform-decls `(body ...))]
         [f (open-output-file "output.tex" #:exists 'replace)])
     (displayln output f)
     (close-output-port f))))
   

(provide (rename-out [module-begin #%module-begin]))

;;(displayln
;; (transform-decls
;;  '(
