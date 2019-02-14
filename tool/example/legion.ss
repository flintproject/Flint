;; http://www.scholarpedia.org/article/LEGION:_locally_excitatory_globally_inhibitory_oscillator_networks
(define-model legion
  (variable t :independent)
  (variable x)
  (variable y)
  (parameter alpha :default 20)
  (parameter beta :default 1)
  (parameter epsilon :default 1/100)
  (parameter I :default 1)
  (eq (diff (bvar t) x)
      (plus (minus (times 3 x) (power x 3))
            (minus 2 y)
            I))
  (eq (diff (bvar t) y)
      (times epsilon
             (minus (times alpha
                           (plus 1 (tanh (divide x beta))))
                    y)))
  )
