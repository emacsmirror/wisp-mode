#!/usr/bin/env sh
exec guile -L $(dirname $(dirname $(realpath "$0"))) --language=wisp -e '(@@ (examples ensemble-estimation) main)' -s "$0" "$@"
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

define-module : examples ensemble-estimation 
use-modules : srfi srfi-42 ; list-ec
use-modules 
  : ice-9 popen
        . #:select : open-output-pipe close-pipe

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

define : mean l
       . "Calculate the average value of l (numbers)."
       / : apply + l
           length l
       

define : standard-deviation l
       . "Calculate the standard deviation of list l (numbers)."
       let : : l_mean : mean l
         sqrt
           / : sum-ec (: i l) : expt {i - l_mean} 2
             . {(length l) - 1}

define : standard-deviation-from-deviations . l
       . "Calculate the standard deviation from a list of deviations (x - x_mean)."
       sqrt 
         / : sum-ec (: i l) : expt i 2
           . {(length l) - 1}

define* : write-multiple . x
        . "Helper to avoid suffering from write-newline-typing."
        map : lambda (x) (write x) (newline)
            . x

;; Start with the simple case: One variable and independent observations (R diagonal)
;; First define a truth
define x^seed '(0.5 0.6 7 0.1 0.7 0.9 0.8 0.4)
define x^true : append-ec (: i (length x^seed)) : list-ec (: j x^seed) : * j : list-ref x^seed i
;; And add an initial guess of the parameters
define x^b : append-ec (: i (length x^seed))  '(1 1 1 1 1 1 1 1) ; initial guess
define P : make-covariance-matrix-from-standard-deviations : append-ec (: i (length x^seed)) '(0.5 0.1 0.3 0.1 0.2 0.2 0.2 0.2)

;; Then generate observations
define y⁰-num 3000
define y⁰-pos-max 100
;; At the positions where they are measured. Drawn randomly to avoid
;; giving an undue weight to later values.
define y⁰-pos : list-ec (: i y⁰-num) : * (random:uniform) y⁰-pos-max

;; We need an observation operator to generate observations from true values
define : H x pos
       . "Observation operator. It generates modelled observations from the input.

x are parameters to be optimized, pos is another input which is not optimized. For plain functions it could be the position of the measurement on the x-axis. We currently assume absolute knowledge about the position.
"
       let*
           : len : length x
             ystretch y⁰-pos-max
             x-pos : list-ec (: i len) : * ystretch {{i + 0.5} / {len + 1}}
           apply +
                 list-ec (: i len)
                      * : list-ref x i
                          . pos
                          exp 
                            - 
                              expt 
                                / {pos - (list-ref x-pos i)} {ystretch / 20}
                                . 2


;; We start with true observations which we will disturb later to get
;; the equivalent of measured observations
define y^true : list-ec (: i y⁰-pos) : H x^true i
;; now we disturb the observations with a fixed standard deviation. This assumes uncorrelated observations.
define y⁰-std 50
define y⁰ : list-ec (: i y^true) : + i : * y⁰-std : random:normal
;; and define the covariance matrix. This assumes uncorrelated observations.
define R : make-covariance-matrix-from-standard-deviations : list-ec (: i y⁰-num) y⁰-std

;; Alternative: define observations
;; define y⁰-mean 0.8
;; The actual observations
;; define y⁰ : list-ec (: i y⁰-num) : + y⁰-mean : * y⁰-std : random:normal


define : EnSRT H x P y R y-pos N
       . "Observation function H, parameters x,
parameter-covariance P, observations y, observation covariance R
and number of ensemble members N.

Limitations: y is a single value. R and P are diagonal.
"
       let step
         : observations-to-process y
           observation-variances : list-ec (: i (length y)) : list-ref (list-ref R i) i
           observation-positions y-pos
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
                   y-pos_cur : car observation-positions
                   Hx^b_i
                       list-ec (: i x-deviations) 
                           H 
                             list-ec (: j (length i)) 
                                 + (list-ref x^b j) (list-ref i j)
                             . y-pos_cur
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
                 step
                   cdr observations-to-process
                   cdr observation-variances
                   cdr observation-positions
                   . x^a
                   . x^a-deviations


define : main args
    let*
      : optimized : EnSRT H x^b P y⁰ R y⁰-pos 30
        x-opt : list-ref optimized 0
        x-deviations : list-ref optimized 1
        ; std : sqrt : * {1 / {(length x-deviations) - 1}} : sum-ec (: i x-deviations) : expt i 2
      format #t "x⁰: ~A ± ~A\nx:  ~A ± ~A\nx^t:~A\ny:  ~A ± \ny⁰: ~A ± ~A\nnoise: ~A\n" 
                 . x^b
                 list-ec (: i (length x^b)) : list-ref (list-ref P i) i
                 . x-opt 
                 list-ec (: i (length x-opt))
                    apply standard-deviation-from-deviations : list-ec (: j x-deviations) : list-ref j i
                 . x^true
                 * {1 / (length y⁰)} : apply + : map (lambda (x) (H x-opt x)) y⁰-pos
                 ; apply standard-deviation-from-deviations : map H x-deviations ; FIXME: This only works for trivial H.
                 mean y⁰
                 standard-deviation y⁰
                 . y⁰-std
      ; now plot the result
      let : : port : open-output-pipe "python"
        format port "import pylab as pl\n"
        format port "y0 = [float(i) for i in '~A'[1:-1].split(' ')]\n" y⁰
        format port "ypos = [float(i) for i in '~A'[1:-1].split(' ')]\n" y⁰-pos
        format port "yinit = [float(i) for i in '~A'[1:-1].split(' ')]\n" : list-ec (: i y⁰-pos) : H x^b i
        format port "ytrue = [float(i) for i in '~A'[1:-1].split(' ')]\n" : list-ec (: i y⁰-pos) : H x^true i
        format port "yopt = [float(i) for i in '~A'[1:-1].split(' ')]\n" : list-ec (: i y⁰-pos) : H x-opt i
        format port "pl.plot(*zip(*sorted(zip(ypos, yinit))), label='prior')\n"
        format port "pl.plot(*zip(*sorted(zip(ypos, ytrue))), label='true')\n"
        format port "pl.plot(*zip(*sorted(zip(ypos, yopt))), label='optimized')\n"
        format port "pl.plot(*zip(*sorted(zip(ypos, y0))), marker='+', linewidth=0, label='measurements')\n"
        format port "pl.legend()\n"
        format port "pl.xlabel('position [arbitrary units]')\n"
        format port "pl.ylabel('value [arbitrary units]')\n"
        format port "pl.title('ensemble optimization results')\n"
        format port "pl.show()\n"
        format port "exit()\n"
        close-pipe port
