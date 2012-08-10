;; Looping calculation and insertion code.

(defun picasm-calculate-picloops (seconds freq)
  "Calculate the required values for counterA, counterB, counterC
to get a loop of `SECONDS' seconds, given the PIC runs at `FREQ'
MHz."
  ;; In a single loop, the loop body takes 3 cycles and there are 3
  ;; extra cycles at top and bottom. The whole lot gives:
  ;;   N = counterA*3 + 3
  ;; For a double loop, the inner loop body takes 3 cycles. This gets run 256
  ;; times except for on the first time through (when it runs counterA
  ;; times). There are two extra cycles in the outer loop and then 5 for setup
  ;; and return:
  ;;   N = (256*3+2)*counterB + (counterA*3+2) + 5
  ;; The triple loop has a slightly different structure: it calls a double loop
  ;; (set up with counterA and counterB) counterC times then returns (setup and
  ;; return give an extra 3 cycles), so the equation is very similar:
  ;;   N = counterC*((256*3+2)*counterB + (counterA*3+2) + 5) + 3
  (let* ((cycles (floor (/ (* seconds freq 1e6) 4.0)))
         (one-stage-max (+ (* 256 3) 3))
         (two-stage-max (+ (* (1- one-stage-max) 256) 5))
         (three-stage-max (+ (* two-stage-max 256) 3)))
    (cond
     ((<= cycles one-stage-max)
      (list (floor (- (/ cycles 3) 1))))
     ((<= cycles two-stage-max)
      ;; Rearranging the equation for N, you get that
      ;;   counterB = (cycles-counterA*3+763)/770
      ;; and notice that 0 < 3*counterA < 770, so we can get the right answer by
      ;; rounding down
      ;;   counterB = floor(cycles + 762.5, 770)
      ;; Now counterA = (cycles + 763 - 770*counterB)/3
      (let ((counter-b (floor (+ cycles 762.5) 770)))
        (list (floor (+ cycles 763 (- (* 770 counter-b))) 3)
              (floor (+ cycles 762.5) 770))))
     ;; A triple loop. To find a counterC, notice that the complicated product
     ;; is at most two-stage-max, and can be made as big below that as
     ;; desired. As such,
     ;;   counterC = floor(N-3, two-stage-max)
     ;; will give a sensible value (as small as possible, in fact). Now
     ;; rearrange as before for counterB and counterA, except subtract a further
     ;; 5 because of the delay for call and return.
     ((<= cycles three-stage-max)
      (let* ((counter-c (ceiling (- cycles 3) two-stage-max))
             (cycles-per-call (floor (- cycles 3) counter-c))
             (counter-b (floor (+ cycles-per-call 757.5) 770)))
        (list (floor (+ cycles-per-call 758 (- (* 770 counter-b))) 3)
              counter-b
              counter-c)))
     (t
      (error "The wait is too long for a triple-nested loop.")))))

(defun picloops-loop-1 (label counter-a)
  "Initializes and outputs a single-stage loop"
  (macrolet ((i-f (str &rest args)
		  `(insert (format ,str ,@args))))
    (i-f "\ndelay_%s:\n" label)
    (i-f "\tMOVLW\tD'%d'\n" counter-a)
    (i-f "\tMOVWF\tCounterA\n")
    (i-f "delay_%s_loop:\n" label)
    (i-f "\tDECFSZ\tCounterA,f\n")
    (i-f "\tGOTO\tdelay_%s_loop\n" label)
    (i-f "\tRETURN\n")))

(defun picloops-loop-2 (label counter-a counter-b)
  "Initializes and outputs a two-stage loop"
  (macrolet ((i-f (str &rest args)
		  `(insert (format ,str ,@args))))
    (i-f "\ndelay_%s:\n" label)
    (i-f "\tMOVLW\tD'%d'\n" counter-b)
    (i-f "\tMOVWF\tCounterB\n")
    (i-f "\tMOVLW\tD'%d'\n" counter-a)
    (i-f "\tMOVWF\tCounterA\n")
    (i-f "delay_%s_loop:\n" label)
    (i-f "\tDECFSZ\tCounterA,f\n")
    (i-f "\tGOTO\tdelay_%s_loop\n" label)
    (i-f "\tDECFSZ\tCounterB,f\n")
    (i-f "\tGOTO\tdelay_%s_loop\n" label)
    (i-f "\tRETURN\n")))

(defun picloops-loop-3 (label counter-a counter-b counter-c)
  "Initializes and outputs a three-stage loop"
  (macrolet ((i-f (str &rest args)
		  `(insert (format ,str ,@args))))  
    (i-f "\ndelay_%s:\n" label)
    (i-f "\tMOVLW\tD'%d'\n" counter-c)
    (i-f "\tMOVWF\tCounterC\n")
    (i-f "delay_%s_loop:\n" label)
    (i-f "\tCALL\tsub_delay_%s\n" label)
    (i-f "\tDECFSZ\tCounterC,f\n")
    (i-f "\tGOTO\tdelay_%s_loop\n" label)
    (i-f "\tRETURN\n\n")
    (i-f "sub_delay_%s:\n" label)
    (i-f "\tMOVLW\tD'%d'\n" counter-b)
    (i-f "\tMOVWF\tCounterB\n")
    (i-f "\tMOVLW\tD'%d'\n" counter-a)
    (i-f "\tMOVWF\tCounterA\n")
    (i-f "sub_delay_%s_loop:\n" label)
    (i-f "\tDECFSZ\tCounterA,f\n")
    (i-f "\tGOTO\tsub_delay_%s_loop\n" label)
    (i-f "\tDECFSZ\tCounterB,f\n")
    (i-f "\tGOTO\tsub_delay_%s_loop\n" label)
    (i-f "\tRETURN\n")))

(provide 'picasm-loops)
