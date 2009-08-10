(load "controller.scm")

(define STEPS 25000)

;; Controller program for the Eccentric Meet and Greet problem.

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

;; 0. Populate VM's memory areas

(system "./decompile-obf.py bin/bin3.obf > ./scratch/emeetandgreet-data.scm")

(populate-memory! "./scratch/emeetandgreet-data.scm")

;; We can find the target satellite's position wrt Earth by adding our
;; position vector wrt Earth with the target's position wrt us (which
;; is the *reverse* of our position wrt the target).  Yay vector
;; addition!

;; FIXME: for Eccentric Meet and Greet this might not work, since we
;; have to deal with ellipses.

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
      [3001 (run-scenario 3001)]
      [3002 (run-scenario 3002)]
      [3003 (run-scenario 3003)]
      [3004 (run-scenario 3004)])))

(define run-scenario
  (lambda (scenario)
    (let ([p (open-output-file "./scratch/emeetandgreet.log" 'replace)]
          [coords (open-output-file "./scratch/emeetandgreet.coords" 'replace)]
          [trace (open-output-file "./scratch/emeetandgreet.trace" 'replace)]
          [input-data `((2 . 0) (3 . 0) (16000 . ,scenario))])
      (letrec ([loop
                (lambda (n)
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
                        (let ([cor (orbit-radius
                                    (read-output-port 's_x_wrt_earth)
                                    (read-output-port 's_y_wrt_earth))])
                          (let-values ([(s_x s_y tor) (target-pos)])
                            (log-output
                             n
                             p
                             `(meetandgreet ,cor ,tor ,s_x ,s_y)))

                          ;; Logging location.
                          (if (= (modulo n 100) 0)
                              (log-location n
                                            (read-output-port 's_x_wrt_earth)
                                            (read-output-port 's_y_wrt_earth)
                                            coords))

                          ;; 4. potentially generate new input port
                          ;; values, based on scenario
                          (pmatch scenario
                            [3001 (void)]

                            [3002 (void)]

                            [3003 (void)]

                            [3004 (void)])

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

      (show-visualization 'emeetandgreet))))

(printf "usage: (controller <scenario-number>)\n")
