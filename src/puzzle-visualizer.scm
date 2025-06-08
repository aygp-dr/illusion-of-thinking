#!/usr/bin/env guile
;; Interactive visualization of puzzle complexity effects on LRM performance
;; Demonstrates key findings from "The Illusion of Thinking" paper

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 format))

;;; Define record types for cleaner data management
(define-record-type <puzzle-instance>
  (make-puzzle-instance name size moves difficulty)
  puzzle-instance?
  (name puzzle-name)
  (size puzzle-size)
  (moves puzzle-moves)
  (difficulty puzzle-difficulty))

(define-record-type <model-result>
  (make-model-result accuracy tokens first-error)
  model-result?
  (accuracy result-accuracy)
  (tokens result-tokens)
  (first-error result-first-error))

;;; Complexity calculations for each puzzle type
(define (calculate-moves puzzle-type n)
  "Calculate minimum moves required for puzzle of size n"
  (case puzzle-type
    ((tower-hanoi) (- (expt 2 n) 1))
    ((checker-jumping) (- (expt (+ n 1) 2) 1))
    ((river-crossing) (* 2 n 3))  ; Approximation
    ((blocks-world) (* n 2))       ; Approximation
    (else 0)))

;;; Model behavior simulation based on paper findings
(define (simulate-model-performance model-type puzzle-type complexity)
  "Simulate model performance showing the three regimes"
  (let* ((is-thinking? (eq? model-type 'lrm-thinking))
         ;; Different collapse points for different puzzles
         (collapse-point
          (case puzzle-type
            ((tower-hanoi) (if is-thinking? 12 10))
            ((checker-jumping) (if is-thinking? 8 6))
            ((river-crossing) (if is-thinking? 5 3))
            ((blocks-world) (if is-thinking? 10 8))))
         
         ;; Calculate accuracy based on regime
         (accuracy
          (cond
           ;; Low complexity regime
           ((< complexity 3)
            (if is-thinking?
                (max 0 (- 90 (* complexity 15)))  ; Overthinking penalty
                (max 0 (- 100 (* complexity 10)))))
           
           ;; Medium complexity regime
           ((< complexity (- collapse-point 2))
            (if is-thinking?
                (max 0 (- 100 (* complexity 7)))
                (max 0 (- 95 (* complexity 12)))))
           
           ;; Approaching collapse
           ((< complexity collapse-point)
            (if is-thinking?
                (max 0 (- 60 (* (- complexity (- collapse-point 2)) 30)))
                0))
           
           ;; Complete collapse
           (else 0)))
         
         ;; Thinking tokens showing scaling limit
         (tokens
          (if is-thinking?
              (let ((peak (- collapse-point 2)))
                (cond
                 ((< complexity peak)
                  (* complexity complexity 2000))
                 ((< complexity collapse-point)
                  (* (- (* peak 2) complexity) 1500))
                 (else 500)))
              0))
         
         ;; First error position
         (first-error
          (if (> accuracy 0)
              (if is-thinking?
                  (min complexity (* complexity 10))
                  (min complexity (* complexity 5)))
              1)))
    
    (make-model-result accuracy tokens first-error)))

;;; ASCII art visualization
(define (draw-performance-graph puzzle-type max-n)
  "Draw ASCII graph showing performance across complexity"
  (format #t "~%Performance Graph: ~a~%" puzzle-type)
  (format #t "100 |~%")
  
  ;; Draw accuracy lines
  (do ((y 90 (- y 10)))
      ((< y 0))
    (format #t "~3d |" y)
    (do ((n 1 (+ n 1)))
        ((> n max-n))
      (let* ((lrm-result (simulate-model-performance 'lrm-thinking puzzle-type n))
             (std-result (simulate-model-performance 'standard-llm puzzle-type n))
             (lrm-acc (result-accuracy lrm-result))
             (std-acc (result-accuracy std-result)))
        (cond
         ((and (>= lrm-acc y) (< lrm-acc (+ y 10)))
          (display "T"))
         ((and (>= std-acc y) (< std-acc (+ y 10)))
          (display "S"))
         ((and (>= lrm-acc y) (>= std-acc y))
          (display "*"))
         (else
          (display " ")))))
    (newline))
  
  (format #t "  0 |")
  (do ((n 1 (+ n 1)))
      ((> n max-n))
    (display "-"))
  (newline)
  (format #t "    ")
  (do ((n 1 (+ n 1)))
      ((> n max-n))
    (if (= (modulo n 5) 0)
        (format #t "~d" (modulo n 10))
        (display " ")))
  (newline)
  (format #t "    Complexity (N)~%")
  (format #t "    T=Thinking LRM, S=Standard LLM, *=Both~%~%"))

;;; Show thinking token scaling behavior
(define (show-token-scaling puzzle-type)
  "Visualize the counterintuitive token scaling behavior"
  (format #t "Thinking Token Usage - ~a:~%" puzzle-type)
  (format #t "Complexity | Tokens  | Visual~%")
  (format #t "-----------|---------|")
  (do ((i 0 (+ i 1))) ((= i 50)) (display "-"))
  (newline)
  
  (do ((n 1 (+ n 1)))
      ((> n 15))
    (let* ((result (simulate-model-performance 'lrm-thinking puzzle-type n))
           (tokens (result-tokens))
           (bar-length (/ tokens 1000)))
      (format #t "    ~2d     | ~6d | " n tokens)
      (do ((i 0 (+ i 1)))
          ((>= i bar-length))
        (display "█"))
      (newline))))

;;; Interactive demonstration menu
(define (interactive-demo)
  "Run interactive demonstration of key findings"
  (format #t "~%=== LRM Complexity Analysis Interactive Demo ===~%~%")
  (format #t "Key Findings from 'The Illusion of Thinking':~%")
  (format #t "1. Three performance regimes (low/medium/high complexity)~%")
  (format #t "2. Thinking token scaling limit~%")
  (format #t "3. Algorithm execution failure~%")
  (format #t "4. Cross-puzzle inconsistencies~%~%")
  
  ;; Demonstrate each puzzle
  (for-each (lambda (puzzle)
              (draw-performance-graph puzzle 15)
              (show-token-scaling puzzle)
              (newline))
            '(tower-hanoi checker-jumping river-crossing blocks-world))
  
  ;; Show cross-puzzle comparison
  (format #t "~%Cross-Puzzle Performance at Fixed Compositional Depths:~%")
  (format #t "Moves | Tower-Hanoi | Checker-Jump | River-Cross | Blocks-World~%")
  (format #t "------|-------------|--------------|-------------|-------------~%")
  
  (for-each (lambda (target-moves)
              (format #t "~5d |" target-moves)
              (for-each (lambda (puzzle)
                          (let* ((n (find (lambda (i)
                                           (>= (calculate-moves puzzle i) target-moves))
                                         (iota 20 1)))
                                 (result (if n
                                           (simulate-model-performance 'lrm-thinking puzzle n)
                                           #f)))
                            (if result
                                (format #t " ~3d% (N=~2d) |" 
                                       (result-accuracy result)
                                       (or n 0))
                                (format #t "     N/A     |"))))
                        '(tower-hanoi checker-jumping river-crossing blocks-world))
              (newline))
            '(10 30 50 100 200)))

;;; Mermaid diagram generator for paper concepts
(define (generate-mermaid-diagram)
  "Generate Mermaid diagram showing the three regimes"
  (format #t "~%~%Mermaid Diagram - Three Performance Regimes:~%")
  (format #t "```mermaid~%")
  (format #t "graph TD~%")
  (format #t "    A[Problem Complexity] --> B{Complexity Level}~%")
  (format #t "    B -->|Low: 1-3| C[Low Complexity Regime]~%")
  (format #t "    B -->|Medium: 4-8| D[Medium Complexity Regime]~%")
  (format #t "    B -->|High: 9+| E[High Complexity Regime]~%")
  (format #t "    ~%")
  (format #t "    C --> F[Standard LLM Better]~%")
  (format #t "    C --> G[LRM Overthinks]~%")
  (format #t "    ~%")
  (format #t "    D --> H[LRM Excels]~%")
  (format #t "    D --> I[Thinking Helps]~%")
  (format #t "    ~%")
  (format #t "    E --> J[Both Collapse]~%")
  (format #t "    E --> K[Token Reduction]~%")
  (format #t "    ~%")
  (format #t "    style C fill:#90EE90~%")
  (format #t "    style D fill:#87CEEB~%")
  (format #t "    style E fill:#FFB6C1~%")
  (format #t "```~%"))

;; Run the interactive demo
(interactive-demo)
(generate-mermaid-diagram)
