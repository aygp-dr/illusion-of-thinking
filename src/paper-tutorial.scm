#!/usr/bin/env guile
!#
;;; Tutorial on key concepts from 'The Illusion of Thinking' paper
;;; This script provides a guided exploration of the paper's findings
;;; using Guile Scheme, with examples of the puzzle simulators

(use-modules (ice-9 format)            ; For formatted output
             (ice-9 textual-ports)     ; For file reading
             (ice-9 regex)             ; For text processing
             (ice-9 match)             ; For pattern matching
             (srfi srfi-1))            ; List library

;;; Configuration
(define paper-text-file "paper/the-illusion-of-thinking.txt")
(define display-width 78)

;;; Utility functions
(define (check-paper-exists)
  "Check if the paper text file exists"
  (when (not (file-exists? paper-text-file))
    (format #t "Error: Could not find the extracted paper text at ~a~%" paper-text-file)
    (format #t "Please run 'make extract' first to extract the text from the PDF.~%")
    (exit 1)))

(define (load-paper-text)
  "Load the extracted paper text"
  (call-with-input-file paper-text-file get-string-all))

(define (print-header title)
  "Print a formatted section header"
  (let ((padding (make-string (quotient (- display-width (string-length title)) 2) #\space)))
    (format #t "~%~a~%" (make-string display-width #\=))
    (format #t "~a~a~a~%" padding title padding)
    (format #t "~a~%~%" (make-string display-width #\=))))

(define (print-section title content)
  "Print a formatted section"
  (format #t "~%--- ~a ---~%~%" title)
  (display content)
  (newline))

(define (wrap-text text width)
  "Wrap text to specified width"
  (let* ((words (string-split text #\space))
         (lines '())
         (current-line ""))
    (for-each 
     (lambda (word)
       (let ((test-line (string-append current-line (if (string-null? current-line) "" " ") word)))
         (if (> (string-length test-line) width)
             (begin
               (set! lines (cons current-line lines))
               (set! current-line word))
             (set! current-line test-line))))
     words)
    (when (not (string-null? current-line))
      (set! lines (cons current-line lines)))
    (string-join (reverse lines) "\n")))

;;; Paper analysis functions
(define (extract-key-phrases text patterns)
  "Extract text that matches patterns"
  (let ((results '()))
    (for-each
     (lambda (pattern)
       (let ((matches (list-matches pattern text)))
         (when (not (null? matches))
           (for-each
            (lambda (match)
              (let ((matched-text (match:substring match 1)))
                (when matched-text
                  (set! results (cons (string-trim-both matched-text) results)))))
            matches))))
     patterns)
    (reverse results)))

;;; Tower of Hanoi - Optimal solution generator
(define (hanoi-generate-moves n source target auxiliary)
  "Generate optimal moves for Tower of Hanoi"
  (if (= n 1)
      (list (list n source target))
      (append (hanoi-generate-moves (- n 1) source auxiliary target)
              (list (list n source target))
              (hanoi-generate-moves (- n 1) auxiliary target source))))

;;; Checker Jumping - State visualization and move validation
(define (make-checker-state n)
  "Create initial Checker Jumping state with n red and n blue checkers"
  (append (make-list n 'R) '(_) (make-list n 'B)))

(define (display-checker-state state)
  "Display the checker board state"
  (format #t "  ~{~a ~}~%" (iota (length state)))
  (format #t "[ ~{~a ~}]~%" state))

(define (checker-valid-move? board color from to)
  "Check if a checker move is valid"
  (let ((size (length board)))
    (and (>= from 0) (< from size)
         (>= to 0) (< to size)
         (eq? (list-ref board from) color)
         (eq? (list-ref board to) '_)
         (case color
           ((R) (> to from))  ; Red moves right
           ((B) (< to from))) ; Blue moves left
         (let ((distance (abs (- to from))))
           (or (= distance 1)  ; Slide
               (and (= distance 2)  ; Jump
                    (not (eq? (list-ref board (/ (+ from to) 2)) '_))
                    (not (eq? (list-ref board (/ (+ from to) 2)) color))))))))

(define (checker-apply-move board color from to)
  "Apply a move to checker board"
  (let ((new-board (list-copy board)))
    (list-set! new-board from '_)
    (list-set! new-board to color)
    new-board))

;;; Performance regime simulation
(define (simulate-lrm-accuracy complexity puzzle-type model-type)
  "Simulate LRM accuracy based on complexity"
  (let* ((base-threshold (case puzzle-type
                          ((tower-hanoi) 10)
                          ((checker-jumping) 6)
                          ((river-crossing) 4)
                          ((blocks-world) 8)))
         (thinking-bonus (if (eq? model-type 'thinking) 2 0))
         (effective-threshold (+ base-threshold thinking-bonus)))
    (cond
     ;; Low complexity regime
     ((< complexity 3)
      (if (eq? model-type 'thinking)
          (- 100 (* complexity 10))  ; Thinking models overthink
          (- 100 (* complexity 5)))) ; Standard models more efficient
     
     ;; Medium complexity regime
     ((< complexity effective-threshold)
      (if (eq? model-type 'thinking)
          (- 100 (* complexity 8))
          (- 100 (* complexity 15))))
     
     ;; High complexity regime
     (else 0))))

;;; Token usage simulation
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
     
     ;; Counterintuitive decline
     ((< complexity (* peak-complexity 2))
      (* (- (* peak-complexity 2) complexity) 800))
     
     ;; Minimal effort
     (else 1000))))

;;; Main tutorial function
(define (run-tutorial)
  "Run the full tutorial on the paper"
  (check-paper-exists)
  (let ((paper-text (load-paper-text)))
    ;; Introduction
    (print-header "THE ILLUSION OF THINKING - PAPER TUTORIAL")
    
    (print-section "Introduction" 
      (wrap-text
       "This tutorial explores key concepts from 'The Illusion of Thinking' paper by Apple researchers. 
        The paper investigates the limitations of Large Reasoning Models (LRMs) across different 
        complexity regimes and puzzle environments. We'll demonstrate these concepts with 
        Scheme implementations of the puzzles and performance simulations."
       display-width))
    
    ;; Tower of Hanoi demonstration
    (print-section "Tower of Hanoi Demonstration" 
      (format #t "~
Tower of Hanoi is a classic puzzle with exponential complexity.
For n=3 disks, the optimal solution requires 7 moves:~%~%"))
    
    (let ((moves (hanoi-generate-moves 3 0 2 1)))
      (for-each
       (lambda (move idx)
         (format #t "~2d. Move disk ~a from peg ~a to peg ~a~%" 
                 idx (car move) (cadr move) (caddr move)))
       moves
       (iota (length moves) 1)))
    
    ;; Checker Jumping demonstration
    (print-section "Checker Jumping Demonstration"
      (format #t "~
Checker Jumping is a puzzle with quadratic complexity.
Initial state for n=2:~%~%"))
    
    (let ((state (make-checker-state 2)))
      (display-checker-state state)
      (newline)
      
      (format #t "Valid moves for Red checkers:~%")
      (for-each
       (lambda (from)
         (for-each
          (lambda (to)
            (when (checker-valid-move? state 'R from to)
              (format #t "- Move R from ~a to ~a~%" from to)))
          (iota (length state))))
       (iota (length state)))
      (newline)
      
      (format #t "Valid moves for Blue checkers:~%")
      (for-each
       (lambda (from)
         (for-each
          (lambda (to)
            (when (checker-valid-move? state 'B from to)
              (format #t "- Move B from ~a to ~a~%" from to)))
          (iota (length state))))
       (iota (length state))))
    
    ;; Performance regimes demonstration
    (print-section "Performance Regimes Demonstration"
      (format #t "~
The paper describes three distinct performance regimes for LRMs
based on problem complexity. Here's a simulation of performance
across complexity levels for Tower of Hanoi:~%~%"))
    
    (format #t "N\tStandard\tThinking\tTokens~%")
    (format #t "---------------------------------------------~%")
    
    (for-each
     (lambda (n)
       (let ((standard-acc (simulate-lrm-accuracy n 'tower-hanoi 'standard))
             (thinking-acc (simulate-lrm-accuracy n 'tower-hanoi 'thinking))
             (tokens (simulate-thinking-tokens n 'tower-hanoi)))
         (format #t "~a\t~a%\t\t~a%\t\t~a~%"
                 n standard-acc thinking-acc tokens)))
     '(1 2 3 4 5 6 7 8 10 12 15))
    
    ;; Key findings summary
    (print-section "Key Findings Summary"
      (wrap-text "
1. Three Distinct Regimes: Performance varies dramatically with complexity
   - Low (1-3): Standard LLMs outperform 'thinking' models
   - Medium (4-8): Thinking provides advantage
   - High (9+): Universal collapse to zero accuracy

2. Scaling Limit: Counterintuitively, models reduce reasoning effort as problems
   approach critical complexity

3. Algorithm Failure: Even with explicit step-by-step instructions, models fail
   at the same complexity points

4. Inconsistent Reasoning: Models can handle 100+ moves in one puzzle but fail
   at 5 moves in another

These findings challenge assumptions about LRM capabilities and suggest
fundamental barriers to generalizable reasoning in current approaches."
       display-width))
    
    ;; Conclusion
    (print-section "Interactive Exploration" 
      (wrap-text "
To explore these concepts further:
- Run 'make run' to see Python-based puzzle simulations
- Examine the Scheme implementations in src/puzzle-simulators.scm
- Extract the paper text with 'make extract' and explore it yourself

The paper suggests that current LRMs exhibit an 'illusion of thinking' rather
than true generalizable reasoning capabilities. Understanding these limitations
is crucial for advancing AI research toward more robust reasoning systems."
       display-width))))

;; Run the tutorial
(run-tutorial)