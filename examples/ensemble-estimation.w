#!/usr/bin/env sh
exec guile -L ~/wisp --language=wisp "$0" "$@"
; !#

;; Simple Ensemble Square Root Filter to estimate function parameters
;; based on measurements.

;; Provide first guess parameters x^b and measurements y⁰ to get
;; optimized parameters x^a.

;; Method
;; x^b = '(…) ; first guess of the parameters
;; P = '((…) (…) …) ; parameter covariance
;; y⁰ = '(…) ; observations
;; R = '((…) (…) …) ; observation covariance
;; H: H(x) → y ; provide modelled observations for the given parameters. Just run the function.
;; with N ensemble members (i=1, … N) drawn from the state x^b:
;; For each measurement y⁰_j:
;;     x'^b: X = 1/√(N-1)(x'b_1, …, x'b_N)^T
;;     with P = XX^T ; in the simplest case x'^b are gaussian
;;                     distributed with standard distribution from
;;                     square root of the diagonals.
;;     x_i = x^b + x'^b_i
;;     H(x^b_i) = H(x^b + x'^b_i)
;;     H(x^b) = (1/N)·Σ H(x^b + x'^b_i)
;;     H(x'^b_i) = H(x^b + x'_i) - H(x^b)
;;     HPHt = 1/(N-1)(H(x'_1), …, H(x'_N))(H(x'1), …, H(x'N))T
;;     PHt = 1/(N-1)(x'_1, …, x'_N)(H(x'1), …, H(x'N))T
;;     K = PHt*(HPHt + R)⁻¹
;;     x^a = x^b + K(y⁰_j - H(x^b))
;;     α = (1 + √(R/(HPHt+R)))⁻¹
;;     x'^a = x'^b - αK·H(x'^b)

use-modules : srfi srfi-42 ; list-ec

; seed the random number generator
set! *random-state* : random-state-from-platform

define : make-diagonal-matrix-with-trace trace
         let : : dim : length trace
             list-ec (: i dim)
               list-ec (: j dim)
                 if : = i j
                      list-ref trace i
                      . 0.0

define : make-covariance-matrix-from-standard-deviations stds
         make-diagonal-matrix-with-trace : map (lambda (x) (expt x 2)) stds

define : standard-deviation-from-deviations . l
       . "Calculate the standard deviation from a list of deviations (x - x_mean)."
       sqrt 
         / : sum-ec (: i l) : expt i 2
           . {(length l) - 1}
         

;; Start with the simple case: One variable and independent observations (R diagonal)
define x^b '(1 1 1 1) ; initial guess
define P : make-covariance-matrix-from-standard-deviations '(0.5 0.1 0.3 0.1)

define y⁰-num 10000
define y⁰ : list-ec (: i y⁰-num) : + 0.8 : * 0.1 : random:normal
define y⁰-std 0.1
define R : make-covariance-matrix-from-standard-deviations : list-ec (: i y⁰-num) y⁰-std

define : H . x
       . "Simple single state observation operator which just returns the sum of the state."
       apply + : list-ec (: i (length x)) : * {i + 1} : list-ref x i

define* : write-multiple . x
        map : lambda (x) (write x) (newline)
            . x

define : EnSRT-single-state H x P y R N
       . "Observation function H, parameters x,
parameter-covariance P, observations y, observation covariance R
and number of ensemble members N.

Limitations: y is a single value. R and P are diagonal.
"
       let process-observation
         : observations-to-process y
           observation-variances : list-ec (: i (length y)) : list-ref (list-ref R i) i
           x^b x
           x-deviations 
               list-ec (: i N)
                 list-ec (: j (length x))
                     * : random:normal
                         sqrt : list-ref (list-ref P j) j ; only for diagonal P!
         cond
            : null? observations-to-process
              list x^b x-deviations
            else
               ; write : list x^b '± : sqrt : * {1 / {(length x-deviations) - 1}} : sum-ec (: i x-deviations) : expt i 2
               ; newline
               let*
                 : y_cur : car observations-to-process
                   R_cur : car observation-variances
                   Hx^b_i 
                       list-ec (: i x-deviations) 
                           apply H 
                               list-ec (: j (length i)) 
                                   + (list-ref x^b j) (list-ref i j)
                   Hx^b 
                      / : sum-ec (: i Hx^b_i) i 
                        . N
                   Hx^b-prime 
                       list-ec (: i N) 
                           - : list-ref Hx^b_i i
                             . Hx^b
                   HPHt 
                      / : sum-ec (: i Hx^b-prime) {i * i}
                        . {N - 1}
                   PHt 
                      list-ec (: j (length x^b)) ; for each x^b_i multiply the state-element and model-deviation for all ensemble members.
                         * {1 / {N - 1}} 
                             sum-ec (: i N) 
                               * : list-ref (list-ref x-deviations i) j ; FIXME: this currently does not use j because I only do length 1 x
                                   list-ref Hx^b-prime i
                   K : list-ec (: i PHt) {i / {HPHt + R_cur}}
                   x^a 
                     list-ec (: j (length x^b))
                       + : list-ref x^b j
                         * : list-ref K j
                           . {y_cur - Hx^b}
                   α-weight-sqrt : sqrt {R_cur / {HPHt + R_cur}}
                   α {1 / {1 + α-weight-sqrt}}
                   x^a-deviations 
                     list-ec (: i N) ; for each ensemble member
                       list-ec (: j (length x^b)) ; and each state variable
                         - : list-ref (list-ref x-deviations i) j
                           * α
                             list-ref K j
                             list-ref Hx^b-prime i
                 process-observation
                   cdr observations-to-process
                   cdr observation-variances
                   . x^a
                   . x^a-deviations

let*
  : optimized : EnSRT-single-state H x^b P y⁰ R 300
    x-opt : list-ref optimized 0
    x-deviations : list-ref optimized 1
    ; std : sqrt : * {1 / {(length x-deviations) - 1}} : sum-ec (: i x-deviations) : expt i 2
  format #t "x:  ~A ± ~A\ny:  ~A ± ~A\ny⁰:  ~A ± ~A" 
             . x-opt 
             list-ec (: i (length x-opt))
                apply standard-deviation-from-deviations : list-ec (: j x-deviations) : list-ref j i
             apply H x-opt
             apply standard-deviation-from-deviations : map (lambda (x) (apply H x)) x-deviations
             * {1 / (length y⁰)} : apply + y⁰ 
             * : sqrt {1 / (length y⁰)} 
               . y⁰-std