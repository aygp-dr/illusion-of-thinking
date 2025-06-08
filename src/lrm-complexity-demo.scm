#!/usr/bin/env guile
;; Demonstration of LRM complexity regimes from "The Illusion of Thinking" paper
;; This simulates the three performance regimes observed in the research

(use-modules (srfi srfi-1)   ; List utilities
             (srfi srfi-43)  ; Vector utilities
             (ice-9 format))

;;; Simulate LRM performance characteristics
(define (simulate-lrm-accuracy complexity puzzle-type model-type)
  "Simulate accuracy based on complexity and model type"
  (let* ((base-threshold (case puzzle-type
                          ((tower-hanoi) 10)
                          ((checker-jumping) 6)
                          ((river-crossing) 4)
                          ((blocks-world) 8)))
         (thinking-bonus (if (eq? model-type 'thinking) 2 0))
         (effective-threshold (+ base-threshold thinking-bonus)))
    (cond
     ;; Low complexity regime - standard models are better
     ((< complexity 3)
      (if (eq? model-type 'thinking)
          (- 100 (* complexity 10))  ; Thinking models overthink
          (- 100 (* complexity 5))))  ; Standard models more efficient
     
     ;; Medium complexity regime - thinking models excel
     ((< complexity effective-threshold)
      (if (eq? model-type 'thinking)
          (- 100 (* complexity 8))
          (- 100 (* complexity 15))))
     
     ;; High complexity regime - both collapse
     (else 0))))

(define (simulate-thinking-tokens complexity puzzle-type)
  "Simulate thinking token usage showing the scaling limit"
  (let ((peak-complexity (case puzzle-type
                          ((tower-hanoi) 8)
                          ((checker-jumping) 5)
                          ((river-crossing) 3)
                          ((blocks-world) 6))))
    (cond
     ;; Initial scaling with complexity
     ((< complexity peak-complexity)
      (* complexity complexity 1000))
     
     ;; Counterintuitive decline after threshold
     ((< complexity (* peak-complexity 2))
      (* (- (* peak-complexity 2) complexity) 800))
     
     ;; Minimal effort at high complexity
     (else 1000))))

;;; Demonstration of the three regimes
(define (demonstrate-regimes)
  (format #t "=== LRM Performance Regimes Demo ===~%~%")
  
  ;; Test across complexity levels
  (let ((complexities '(1 2 3 4 5 6 7 8 10 12 15 20))
        (puzzle 'tower-hanoi))
    
    (format #t "Tower of Hanoi - Accuracy vs Complexity:~%")
    (format #t "N\tStandard\tThinking\tTokens~%")
    (format #t "----------------------------------------~%")
    
    (for-each (lambda (n)
                (let ((standard-acc (simulate-lrm-accuracy n puzzle 'standard))
                      (thinking-acc (simulate-lrm-accuracy n puzzle 'thinking))
                      (tokens (simulate-thinking-tokens n puzzle)))
                  (format #t "~a\t~a%\t\t~a%\t\t~a~%"
                          n standard-acc thinking-acc tokens)))
              complexities)))

;;; Demonstrate algorithmic execution failure
(define (demonstrate-algorithm-failure)
  (format #t "~%=== Algorithm Execution Failure Demo ===~%~%")
  
  ;; Even with explicit algorithm, models fail at same points
  (define (success-with-algorithm? n with-algo?)
    (let ((base-threshold 10)
          (algo-bonus (if with-algo? 0 0))) ; No improvement!
      (< n (+ base-threshold algo-bonus))))
  
  (format #t "Tower of Hanoi - Success with/without algorithm:~%")
  (format #t "N\tWithout Algo\tWith Algo~%")
  (format #t "--------------------------------~%")
  
  (for-each (lambda (n)
              (format #t "~a\t~a\t\t~a~%"
                      n
                      (if (success-with-algorithm? n #f) "✓" "✗")
                      (if (success-with-algorithm? n #t) "✓" "✗")))
            '(3 5 7 8 9 10 11 12 15)))

;;; Demonstrate inconsistent reasoning across puzzles
(define (demonstrate-puzzle-inconsistency)
  (format #t "~%=== Puzzle Inconsistency Demo ===~%~%")
  
  (define puzzles
    '((tower-hanoi . ((moves . 127) (n . 7) (success . #t)))
      (river-crossing . ((moves . 11) (n . 3) (success . #f)))
      (checker-jumping . ((moves . 24) (n . 4) (success . #t)))
      (blocks-world . ((moves . 15) (n . 10) (success . #f)))))
  
  (format #t "Puzzle Performance Inconsistencies:~%")
  (format #t "Puzzle\t\t\tMoves\tN\tSuccess~%")
  (format #t "--------------------------------------------~%")
  
  (for-each (lambda (puzzle-data)
              (let* ((name (car puzzle-data))
                     (data (cdr puzzle-data))
                     (moves (assq-ref data 'moves))
                     (n (assq-ref data 'n))
                     (success (assq-ref data 'success)))
                (format #t "~a\t~a\t~a\t~a~%"
                        (format #f "~15a" name)
                        moves n 
                        (if success "✓" "✗"))))
            puzzles)
  
  (format #t "~%Note: Model handles 127 moves in Tower of Hanoi but fails at 11 moves in River Crossing!~%"))

;;; Simulate reasoning trace patterns
(define (demonstrate-reasoning_traces complexity)
  (format #t "~%=== Reasoning Trace Patterns (Complexity: ~a) ===~%~%" complexity)
  
  (cond
   ;; Low complexity - overthinking
   ((< complexity 4)
    (format #t "Pattern: OVERTHINKING~%")
    (format #t "Trace: Found solution at position 0.2, but continued exploring...~%")
    (format #t "- Position 0.2: ✓ Correct solution found~%")
    (format #t "- Position 0.4: ✗ Exploring incorrect path~%")
    (format #t "- Position 0.6: ✗ Another incorrect attempt~%")
    (format #t "- Position 0.8: ✗ Still searching...~%")
    (format #t "- Position 1.0: Returns to correct solution~%"))
   
   ;; Medium complexity - eventual success
   ((< complexity 8)
    (format #t "Pattern: EXPLORATION BEFORE SUCCESS~%")
    (format #t "Trace: Explores incorrect paths before finding solution~%")
    (format #t "- Position 0.2: ✗ Incorrect attempt~%")
    (format #t "- Position 0.4: ✗ Another failure~%")
    (format #t "- Position 0.6: ✗ Getting closer...~%")
    (format #t "- Position 0.8: ✓ Correct solution found!~%")
    (format #t "- Position 1.0: Finalizes solution~%"))
   
   ;; High complexity - complete failure
   (else
    (format #t "Pattern: COMPLETE COLLAPSE~%")
    (format #t "Trace: No correct solutions found~%")
    (format #t "- Position 0.2: ✗ Failed attempt~%")
    (format #t "- Position 0.4: ✗ Another failure~%")
    (format #t "- Position 0.6: ✗ Reasoning degrades~%")
    (format #t "- Position 0.8: ✗ Token usage decreases~%")
    (format #t "- Position 1.0: ✗ Gives up~%"))))

;;; Run all demonstrations
(define (run-all-demos)
  (demonstrate-regimes)
  (demonstrate-algorithm-failure)
  (demonstrate-puzzle-inconsistency)
  (demonstrate-reasoning_traces 2)   ; Low complexity
  (demonstrate-reasoning_traces 6)   ; Medium complexity
  (demonstrate-reasoning_traces 15)) ; High complexity

;; Execute demonstrations
(run-all-demos)
