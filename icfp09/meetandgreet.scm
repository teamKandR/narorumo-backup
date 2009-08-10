(load "controller.scm")
(load "deltav.scm")

(define STEPS 200000)

;; Controller program for the Meet and Greet problem.

(define input-port-mapping
  '((2     . delta_v_x)
    (3     . delta_v_y)
    (16000 . configuration)))

(define output-port-mapping
  '((0 . score)
    (1 . fuel_remaining)
    (2 . s_x_wrt_earth)
    (3 . s_y_wrt_earth)
    (4 . s_x_wrt_target)
    (5 . s_y_wrt_target)))

(define second-impulse-fired #f)
(set! second-impulse-fired #f)

(define first-impulse-fired #f)
(set! first-impulse-fired #f)

(define no-longer-close #f)
(set! no-longer-close #f)

;; 0. Populate VM's memory areas

(system "./decompile-obf.py bin/bin2.obf > ./scratch/meetandgreet-data.scm")

(populate-memory! "./scratch/meetandgreet-data.scm")

;; We can find the target satellite's position wrt Earth by adding our
;; position vector wrt Earth with the target's position wrt us (which
;; is the *reverse* of our position wrt the target).  Yay vector
;; addition!
(define target-pos
  (lambda ()
    (let ([s_x_wrt_earth (read-output-port 's_x_wrt_earth)]
          [s_y_wrt_earth (read-output-port 's_y_wrt_earth)]
          [s_x_wrt_target (read-output-port 's_x_wrt_target)]
          [s_y_wrt_target (read-output-port 's_y_wrt_target)])
      ;; #(s_earth) + -#(s_target) = #(o_target)
      ;; length(#(o_target)) = tor
      (let ([s_x (+ s_x_wrt_earth (- s_x_wrt_target))]
            [s_y (+ s_y_wrt_earth (- s_y_wrt_target))])
        (values s_x s_y (orbit-radius s_x s_y))))))

(define controller
  (lambda (scenario)
    ;; we can potentially pass more precalculated arguments to
    ;; run-scenario for each of the four scenarios.
    (pmatch scenario
      [2001 (run-scenario 0.0005 500 2001)]
      [2002 (run-scenario 0.0005 500 2002)]
      [2003 (run-scenario 0.0001 600 2003)]
      [2004 (run-scenario 0.0005 500 2004)])))

(define run-scenario
  (lambda (tolerance1 tolerance2 scenario)
    (let* ([p (open-output-file "./scratch/meetandgreet.log" 'replace)]
           [coords (open-output-file "./scratch/meetandgreet.coords" 'replace)]
           [trace (open-output-file "./scratch/meetandgreet.trace" 'replace)]
           [input-data `((2 . 0) (3 . 0) (16000 . ,scenario))]
           [psx #f]
           [psy #f]
           [dvx2 #f]
           [dvy2 #f])
      (letrec
          ([loop
            (lambda (n)
              (if (= (modulo n 20000) 0)
                  (printf "step ~d\n" n))

              (if (and (< n STEPS)
                       (= 0.0 (read-output-port 'score)))
                  (begin
                    ;; 1. write to VM's input ports
                    (populate-input-ports! input-data)
                    (log-input n p)
                    (log-input-trace n trace)

                    ;; 2. execute VM
                    (vm)

                    ;; 3. read VM's output ports
                    (let-values ([(s_x s_y tor) (target-pos)])
                      (let* ([x (read-output-port 's_x_wrt_earth)]
                             [y (read-output-port 's_y_wrt_earth)]
                             [cor (orbit-radius x y)]
                             [travel-time
                              (hohmann-transfer-time cor tor)]
                             [them-later (pos-rad s_x s_y travel-time)]
                             [them-now (pos-rad s_x s_y 0)]
                             [us-now (pos-rad x y 0)]
                             [our-rps (rps cor)])

                        (log-output
                         n
                         p
                         `(meetandgreet ,cor ,tor ,s_x ,s_y))

                        ;; Log location of the target
                        (if (= (modulo n 100) 0)
                            (log-location n s_x s_y coords))

                        ;; Log *our* location.
                        (if (= (modulo n 100) 0)
                            (log-location n x y coords))

                        ;; 4. potentially generate new input port
                        ;; values, based on scenario

                        (if (= n 1)
                            (printf "rps: ~f\nour radians at n=1: ~f\n"
                                    our-rps
                                    (atan y x)))

                        (if (= n 2)
                            (printf "our radians at n=2: ~f\n"
                                    (atan y x)))

                        '(if (< (abs (- pi (abs (- us-now them-later))))
                                tolerance1)
                             (begin
                               (display "difference: ")
                               (display (abs (- pi (abs (- us-now them-later)))))
                               (newline)))

                        ;; ideally: we want the difference between
                        ;; us-now and them-later to be pi.
                        (if (and (not first-impulse-fired)
                                 (< (abs (- pi (abs (- us-now them-later))))
                                    tolerance1))
                            (let ([dvx (deltav-x psx psy x y cor tor)]
                                  [dvy (deltav-y psx psy x y cor tor)])
                              (printf "us-now: ~d\n" us-now)
                              (printf "them-later: ~d\n" them-later)

                              (printf "First impulse fired at time ~d\n" n)
                              (printf "We think Hohmann transfer time will be: ~d\n" travel-time)
                              (printf "dvx: ~d\n" dvx)
                              (printf "dvy: ~d\n" dvy)
                              (write-input-port! 'delta_v_x dvx)
                              (write-input-port! 'delta_v_y dvy)
                              (set! dvx2 (deltavp-x psx psy x y cor tor))
                              (set! dvy2 (deltavp-y psx psy x y cor tor))
                              (set! first-impulse-fired n)))

                        (if (and first-impulse-fired
                                 (= n (+ first-impulse-fired 1)))
                            (begin
                              (write-input-port! 'delta_v_x 0)
                              (write-input-port! 'delta_v_y 0)))

                        ;; Print diagnostic info: are we in range?
                        (if (and
                             (< (abs (read-output-port 's_x_wrt_target)) 999)
                             (< (abs (read-output-port 's_y_wrt_target)) 999))
                            (printf "within range at time ~d\n" n))

                        ;; Fire the second impulse if we're close
                        ;; enough to the target.
                        (if (and (not second-impulse-fired)
                                 (and (< (abs (read-output-port 's_x_wrt_target)) tolerance2)
                                      (< (abs (read-output-port 's_y_wrt_target)) tolerance2)))
                            (begin
                              (printf "close to target orbit: ~d\n" n)
                              (printf "OK, should be close to them now.\n")
                              (printf "us: (~f,~f), them: (~f,~f), dist: ~f\n"
                                      x y s_x s_y
                                      (pythag (- x s_x) (- y s_y)))

                              (set! second-impulse-fired n)
                              (write-input-port! 'delta_v_x dvx2)
                              (write-input-port! 'delta_v_y dvy2)))

                        (if (and second-impulse-fired
                                 (= n (+ second-impulse-fired 1)))
                            (begin
                              (printf "Second impulse fired at time ~d\n" n)
                              (printf "Actual Hohmann transfer time was: ~d\n"
                                      (- second-impulse-fired
                                         first-impulse-fired))
                              (write-input-port! 'delta_v_x 0)
                              (write-input-port! 'delta_v_y 0)))

                        (set! psx x)
                        (set! psy y))

                      ;; 5. repeat
                      (loop (+ n 1))))))])
        (loop 0))
      (close-port p)
      (close-port coords)
      (close-port trace)

      (display "final score: " )
      (display (read-output-port 'score)) (newline)
      (display "final fuel remaining: ")
      (display (read-output-port 'fuel_remaining)) (newline)

      (show-visualization 'meetandgreet))))

(printf "usage: (controller <scenario-number>)\n")
