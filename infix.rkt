#lang racket
(require "../common.rkt")
(require "../syntax/syntax.rkt")
(require "../programs.rkt")

(provide texify-expr infix-prog)


(define-table texify-constants
  [l       "l"]
  [PI      "pi"]
  [E       "e"]
  [eps     "eps"]
  [epsilon "eps"]
  [alpha   "alpha"]
  [beta    "beta"]
  [gamma   "gamma"]
  [phi     "phi"]
  [phi1    "phi_1"]
  [phi2    "phi_2"]
  [lambda  "lambda"]
  [lambda1 "lambda_1"]
  [lambda2 "lambda_2"])

; Note that normal string converters ignore idx and
; procedure converters take idx as first arg.
(define (apply-converter conv args [idx #f])
  (cond
    [(string? conv)     (apply format conv args)]
    [(list? conv)       (apply-converter (list-ref conv (length args)) args idx)]
    [(procedure? conv)  (apply conv (if idx (cons idx args) args))]
    [else               (error "Unknown syntax entry" conv)]))

(define parens-precedence '(#t + * fn #f))

(define (parens-< a b)
  (< (index-of parens-precedence a)
     (index-of parens-precedence b)))

; "enclose" is a MathJax extension which may
; not work with standard TeX processors.
(define (tag str idx)
  (let* ([enc (format "(~a)" str)]
         [col (format "(~a)" enc)]
         [css (format "((~a)(~a))" idx col)])
    css))

(define (untag str)
  (format "(~a)" str))

(define ((tag-inner-untag str) idx . args)
  (tag (apply format str (map untag args)) idx))

(define ((tag-infix op) idx arg1 arg2)
  (format "~a ~a ~a" arg1 (tag op idx) arg2))

; self-paren-level : #t --> paren me
;                    #f --> do not paren me
;
; args-paren-level : #t --> do not paren args
;                    #f --> paren args
(define-table texify-operators
  [+    '(#f "+~a" "~a + ~a")
        `(#f ,(tag-inner-untag "+~a")
             ,(tag-infix "+"))
        '+ '+]
  [-    '(#f "-~a" "~a - ~a")
        `(#f ,(tag-inner-untag "-~a")
             ,(tag-infix "-"))
        '+ '+]
  [*    "~a * ~a"
        (tag-infix "*")
        '* '*]
  [/    '(#f "1/(~a)" "(~a)/(~a)")
        `(#f ,(tag-inner-untag "1/~a")
             ,(tag-infix "/"))
        #f #t]

  [sqr      "(~a)^2"
            (lambda (idx a) (format "(~a)^(~a)" a (tag "2" idx)))
            #f #f]
  [cube     "(~a)^3"
            (lambda (idx a) (format "(~a)^(~a)" a (tag "3" idx)))
            #f #f]
  [cotan    "cotan(~a)"
            (tag-inner-untag "cotan(~a)")
            'fn #f]

  [acos      "acos(~a)"
             (tag-inner-untag "acos(~a)")
             'fn #f]
  [acosh     "acosh(~a)"
             (tag-inner-untag "acosh(~a)")
             'fn #f]
  [asin      "asin(~a)"
             (tag-inner-untag "asin(~a)")
             'fn #f]
  [asinh     "asinh(~a)"
             (tag-inner-untag "asinh(~a)")
             'fn #f]
  [atan      "atan(~a)"
             (tag-inner-untag "atan(~a)")
             'fn #f]
  [atan2     "atan2(~a,~a)"
             (tag-inner-untag "atan2(~a,~a)")
             'fn #t]
  [atanh     "atanh(~a)"
             (tag-inner-untag "atanh(~a)")
             'fn #f]
  [cbrt      "cbrt(~a)"
             (tag-inner-untag "cbrt(~a)")
             #f #t]
  [ceil      "ceil(~a)"
             (tag-inner-untag "ceil(~a)")
             #f #t]
  [copysign  "copysign(~a, ~a)"
             (tag-inner-untag "copysign(~a, ~a)")
             #f #t]
  [cos       "cos(~a)"
             (tag-inner-untag "cos(~a)")
             'fn #f]
  [cosh      "cosh(~a)"
             (tag-inner-untag "cosh(~a)")
             'fn #f]
  [erf       "erf(~a)"
             (tag-inner-untag "erf(~a)")
             'fn #f]
  [erfc      "erfc(~a)"
             (tag-inner-untag "erfc(~a)")
             'fn #f]
  [exp       "exp(~a)"
             (tag-inner-untag "exp(~a)")
             #f #t]
  [exp2      "exp2(~a)"
             (tag-inner-untag "exp2(~a)")
             #f #t]
  [expm1     "expm1(~a)"
             (tag-inner-untag "expm1(~a)")
             #f #t]
  [fabs      "fabs(~a)"
             (tag-inner-untag "fabs(~a)")
             #f #t]
  [fdim      "fdim(~a, ~a)"
             (tag-inner-untag "fdim(~a, ~a)")
             #f #t]
  [floor     "floor(~a)"
             (tag-inner-untag "floor(~a)")
             #f #t]
  [fma       "fma(~a,~a,~a)"
             (tag-inner-untag "fma(~a,~a,~a)")
             #f #f]
  [fmax      "fmax(~a, ~a)"
             (tag-inner-untag "fmax(~a, ~a)")
             #f #t]
  [fmin      "fmin(~a, ~a)"
             (tag-inner-untag "fmin(~a, ~a)")
             #f #t]
  [fmod      "fmod(~a,~a)"
             (tag-infix "fmod(~a,~a)")
             #t #f]
  [hypot     "hypot(~a,~a)"
             (tag-inner-untag "hypot(~a,~a)")
             #f #f]
  [j0        "j0(~a)"
             (tag-inner-untag "j0(~a)")
             'fn #f]
  [j1        "j1(~a)"
             (tag-inner-untag "j1(~a)")
             'fn #f]
  [lgamma    "lgamma(~a)"
             (tag-inner-untag "lgamma(~a)")
             'fn #f]
  [log       "log(~a)"
             (tag-inner-untag "log(~a)")
             'fn #f]
  [log10     "log10(~a)"
             (tag-inner-untag "log10(~a)")
             'fn #f]
  [log1p     "log1p(1 + ~a)"
             (tag-inner-untag "log1p(1 + ~a)")
             #f '+]
  [log2      "log2(~a)"
             (tag-inner-untag "log2(~a)")
             'fn #f]
  [logb      "logb(~a)"
             (tag-inner-untag "logb(~a)")
             'fn #f]
  [pow       "pow(~a,~a)"
             (tag-inner-untag "pow(~a,~a)")
             #f #f]
  [remainder "remainder(~a,~a)"
             (tag-inner-untag "remainder(~a,~a)")
             #t #f]
  [rint      "rint(~a)"
             (tag-inner-untag "rint(~a)")
             'fn #f]
  [round     "round(~a)"
             (tag-inner-untag "round(~a)")
             'fn #f]
  [sin       "sin(~a)"
             (tag-inner-untag "sin(~a)")
             'fn #f]
  [sinh      "sinh(~a)"
             (tag-inner-untag "sinh(~a)")
             'fn #f]
  [sqrt      "sqrt(~a)"
             (tag-inner-untag "sqrt(~a)")
             #f #t]
  [tan       "tan(~a)"
             (tag-inner-untag "tan(~a)")
             'fn #f]
  [tanh      "tanh(~a)"
             (tag-inner-untag "tanh(~a)")
             'fn #f]
  [tgamma    "tgamma(~a)"
             (tag-inner-untag "tgamma(~a)")
             'fn #f]
  [trunc     "trunc(~a)"
             (tag-inner-untag "trunc(~a)")
             'fn #f]
  [y0        "y0(~a)"
             (tag-inner-untag "y0(~a)")
             'fn #f]
  [y1        "y1(~a)"
             (tag-inner-untag "y1(~a)")
             'fn #f]

  [if     "~a ? ~a : ~a"
          (lambda (idx a b c)
            (format "~a ~a ~a : ~a" a (tag "?" idx) b c))
          #t #t]
  [=      "~a = ~a"
          (tag-infix "=")
          #f #t]
  [>      "~a > ~a"
          (tag-infix ">")
          #f #t]
  [<      "~a < ~a"
          (tag-infix "<")
          #f #t]
  [>=     "~a >= ~a"
          (tag-infix ">=")
          #f #t]
  [<=     "~a <= ~a"
          (tag-infix "<=")
          #f #t]
  [not    "not ~a"
          (tag-inner-untag "not ~a")
          'fn #f]
  [and    "~a and ~a"
          (tag-infix "and")
          '* '*]
  [or     "~a or ~a"
          (tag-infix "or")
          '+ '+])

(define (collect-branches expr loc)
  (match expr
    [`(if ,cond ,ift ,iff)
     (cons (list cond ift loc)
           (collect-branches iff (cons 3 loc)))]
    [else
     (list (list #t expr loc))]))

;; The highlight ops are an alist of locations to indexes that marks
;; those locations as highlighted with the given location
;; index. highlight-ops and loc/colors are not meant to be used
;; simultaniously.
(define (infix-prog prog
                     #:loc [color-loc #f]
                     #:color [color "red"]
                     #:highlight-ops [highlight-locs '()])
  "Compile the body of a program to math mode TeX."
  (let texify ([expr (program-body prog)] [parens #t] [loc '(2)])
    (format
      (if (and color-loc (equal? (reverse color-loc) loc))
        (format "\\color(~a){~~a}" color)
        "~a")
      (match expr
        [(? exact-integer?)
         (number->string expr)]
        [(? exact-rational?)
         (format "\\frac(~a)(~a)" (numerator expr) (denominator expr))]
        [(? real?)
         (match (string-split (number->string expr) "e")
           [(list num) num]
           [(list significand exp)
            (define num
              (if (equal? significand "1")
                  (format "10^(~a)" exp)
                  (format "~a . 10^(~a)" significand exp)))
            (if (parens-< parens #f) num (format "( ~a )" num))])]
        [(? symbol?)
         (if (hash-has-key? texify-constants expr)
           (car (hash-ref texify-constants expr))
           (symbol->string expr))]
        [`(if ,cond ,ift ,iff)  
          (let ([texed-branches
                  (for/list ([branch (collect-branches expr loc)])
                    (match branch
                           [(list #t bexpr bloc)
                            (format "{~a}else"
                              (texify bexpr #t (cons 2 bloc)))]
                           [(list bcond bexpr bloc)
                            (format "(~a) & if (~a)"
                              (texify bexpr #t (cons 2 bloc))
                              (texify bcond #t (cons 1 bloc)))]))])
            (format "{ ~a }"
                 (string-join texed-branches "  ")))]
        [`(,f ,args ...)
         (match (hash-ref texify-operators f)
           [(list template highlight-template self-paren-level arg-paren-level)
            (let ([texed-args
                    (for/list ([arg args] [id (in-naturals 1)])
                      (texify arg arg-paren-level (cons id loc)))]
                  [hl-loc
                    (assoc (reverse loc) highlight-locs)])
              (format
                ; omit parens if parent contex has lower precedence
                (if (parens-< parens self-paren-level)
                  "~a"
                  "(~a)")
                (if hl-loc
                  (apply-converter highlight-template texed-args (cdr hl-loc))
                  (apply-converter template texed-args))))])]))))

; TODO probably a better way to write this wrapper using
;      make-keyword-procedure and keyword-apply
(define (texify-expr expr
                     #:loc [color-loc #f]
                     #:color [color "red"]
                     #:highlight-ops [highlight-locs '()])
  (infix-prog (expr->prog expr)
               #:loc color-loc
               #:color color
               #:highlight-ops highlight-locs))

(define (exact-rational? r)
  (and (rational? r) (exact? r)))
