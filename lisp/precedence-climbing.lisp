;;; Precedence climbing for parsing arithmetic expressions
;;; http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm

;; besides the shunting-yard algorithm, the classical solution is based on the following EBNF
;;
;; E --> T {( "+" | "-" ) T}
;; T --> F {( "*" | "/" ) F}
;; F --> P ["^" F]
;; P --> v | "(" E ")" | "-" T
;;
;; it has the following problem
;; - The size of the code is proportional to the number of precedence levels.
;; - The speed of the algorithm is proportional to the number of precedence levels.
;; - The number of precedence levels is built in.

;; Precedence climbing solves all the problems while being simpler
;; than the shunting-yard algorithm

;; it's grammar with parametrized tokenizer

;; E --> Exp(0) 
;; Exp(p) --> P {B Exp(q)} 
;; P --> U Exp(q) | "(" E ")" | v
;; B --> "+" | "-"  | "*" |"/" | "^" | "||" | "&&" | "="
;; U --> "-"

;; Exp(p) recognizes expressions which contain no binary operators
;; (other than in parentheses) with precedence less than p

