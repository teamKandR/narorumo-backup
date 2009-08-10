(load "controller.scm")

(define STEPS 25000)

;; Controller program for the Hohmann problem.

(define input-port-mapping
  '((2     . delta_v_x)
    (3     . delta_v_y)
    (16000 . configuration)))

(define output-port-mapping
  '((0 . score)
    (1 . fuel_remaining)
    (2 . s_x)
    (3 . s_y)
    (4 . target_orbit_radius)))

(define second-impulse-fired #f)
(set! second-impulse-fired #f)

(define no-longer-close #f)
(set! no-longer-close #f)

;; 0. Populate VM's memory areas

(system "./decompile-obf.py bin/bin1.obf > ./scratch/hohmann-data.scm")

(populate-memory! "./scratch/hohmann-data.scm")

(define (flying-decisions cor tor dvx2 dvy2 tolerance n)
  (begin
    ;; where are we?
    (if (< (abs (- cor tor)) 1000)
        '(begin
           (display "we're close ")
           (display n)
           (newline))
        (if (and (not no-longer-close) second-impulse-fired)
            (begin
              (set! no-longer-close #t)
              (display "not close anymore ")
              (display n)
              (newline))))

    (if (= n 0)
        (begin
          (display "easing up on the gas!")
          (newline)
          (write-input-port! 'delta_v_x 0)
          (write-input-port! 'delta_v_y 0)))

    (if (and (not second-impulse-fired)
             (< (abs (- cor tor)) tolerance))
        (begin
          (display "close to target orbit: ")
          (display n)
          (newline)
          (set! second-impulse-fired n)
          (write-input-port! 'delta_v_x dvx2)
          (write-input-port! 'delta_v_y dvy2)))

    (if (and second-impulse-fired
             (= n (+ second-impulse-fired 1)))
        (begin
          (display "easing up on the gas!")
          (newline)
          (write-input-port! 'delta_v_x 0)
          (write-input-port! 'delta_v_y 0)))))

(define controller
  (lambda (scenario)
    ;; precalculated delta_v_x and delta_v_y values for each of four
    ;; scenarios.
    (pmatch scenario
      [1001 (run-scenario -4.40950352391 -2466.48207063 1001)]
      [1002 (run-scenario -869.749607401 -867.908477834 1002)]
      [1003 (run-scenario -2.07829526303 -1672.68266835 1003)]
      [1004 (run-scenario -2427.96808243 -4.44187486643 1004)])))

(define run-scenario
  (lambda (delta_v_x delta_v_y scenario)
    (set! second-impulse-fired #f)
    (set! no-longer-close #f)

    (let ([p (open-output-file "./scratch/hohmann.log" 'replace)]
          [coords (open-output-file "./scratch/hohmann.coords" 'replace)]
          [trace (open-output-file "./scratch/hohmann.trace" 'replace)]
          [input-data `((2 . ,delta_v_x) (3 . ,delta_v_y) (16000 . ,scenario))])
      (letrec
          ([loop
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
                    (let ([cor (orbit-radius (read-output-port 's_x)
                                             (read-output-port 's_y))]
                          [tor (read-output-port 'target_orbit_radius)])

                      (log-output n p `(hohmann ,cor ,tor))

                      ;; Logging location.
                      (if (= (modulo n 100) 0)
                          (log-location n
                                        (read-output-port 's_x)
                                        (read-output-port 's_y)
                                        coords))

                      ;; 4. potentially generate new input port
                      ;; values, based on scenario
                      (pmatch scenario
                        [1001
                         (flying-decisions cor tor
                                           2.65114401373
                                           1482.93320122
                                           54.0         ;; tolerance
                                           n)]

                        [1002
                         (flying-decisions cor tor
                                           ;; data for second blast
                                           699.561974959 ;; delta_v_x
                                           698.081107104 ;; delta_v_y
                                           1.0           ;; tolerance
                                           n)]

                        [1003
                         (flying-decisions cor tor
                                           ;; data for second blast
                                           1.5    ;; delta_v_x 1.51473601714
                                           1219.1 ;; delta_v_y 1219.11103204
                                           10.0   ;; tolerance
                                           n)]

                        [1004
                         (flying-decisions cor tor
                                           ;; data for second blast
                                           1496.62056925
                                           2.73801016547
                                           46.0         ;; tolerance
                                           n)]))

                    ;; 5. repeat
                    (loop (+ n 1)))))])
        (loop 0))
      (close-port p)
      (close-port coords)
      (close-port trace)

      (display "final score: " )
      (display (read-output-port 'score)) (newline)
      (display "final fuel remaining: ")
      (display (read-output-port 'fuel_remaining)) (newline)

      (show-visualization 'hohmann))))

(printf "usage: (controller <scenario-number>)\n")
