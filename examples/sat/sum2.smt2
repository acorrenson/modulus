(set-logic ALL)
(declare-const x Int)
(assert (= (+ x x) 2))
(check-sat)