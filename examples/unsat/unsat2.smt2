(set-logic ALL)
(declare-const x Int)
(declare-const y Int)
(assert (= x y))
(assert (= 1 x))
(assert (= 2 y))
(check-sat)
