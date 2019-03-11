;; Model M3 from PNAS March 11, 2014 111 (10) 3883-3888; https://doi.org/10.1073/pnas.1317072111
(define-model maclean2014-m3
  (variable t :independent)
  (variable x_0 :default 0.5)
  (variable x_1)
  (variable x_2)
  (variable x_3)
  (variable y_0 :default 0.5)
  (variable y_1)
  (variable y_2)
  (variable Phi_x)
  (variable Phi_y)
  (parameter alpha_x :default 0.05) ; 0.8
  (parameter alpha_y :default 0.1) ; 0.1
  (parameter beta_x :default 0.9) ; 0.9
  (parameter beta_y :default 0.1) ; 0.2
  (parameter gamma_x :default 0.9) ; 0.8
  (parameter gamma_y :default 0.03) ; 0.2
  (parameter delta_0 :default 0.9) ; 0.7
  (parameter delta_1 :default 0.75) ; 0.1
  (parameter delta_2 :default 0.6) ; 0.1
  (parameter delta_3 :default 0.6) ; 0.9
  (parameter rho_x :default 0.8) ; 0.1
  (parameter rho_y :default 0.05) ; 0.7
  (parameter kappa :default 0.05) ; 0.1
  (eq (diff (bvar t) x_0)
      (times (minus Phi_x 1) delta_0 x_0))
  (eq (diff (bvar t) x_1)
      (minus (times alpha_x x_0)
             (times delta_1 x_1)))
  (eq (diff (bvar t) x_2)
      (minus (times beta_x x_1)
             (times delta_2 x_2)))
  (eq (diff (bvar t) x_3)
      (minus (times gamma_x x_2)
             (times delta_3 x_3)))
  (eq (diff (bvar t) y_0)
      (times (minus Phi_y 1) delta_0 y_0))
  (eq (diff (bvar t) y_1)
      (minus (times alpha_y y_0)
             (times delta_1 y_1)))
  (eq (diff (bvar t) y_2)
      (minus (times beta_y y_1)
             (times delta_2 y_2)))
  (eq (diff (bvar t) y_3)
      (minus (times gamma_y y_2)
             (times delta_3 y_3)))
  (eq Phi_x
      (divide (times (plus rho_x 1) kappa)
              (plus kappa (times rho_x (plus x_0 y_0)))))
  (eq Phi_y
      (divide (times (plus rho_y 1) kappa)
              (plus kappa (times rho_y (plus x_0 y_0)))))
  )
