#!/usr/bin/env guile
;; Puzzle Simulators for LRM Evaluation
;; Based on "The Illusion of Thinking" paper

(use-modules (srfi srfi-1)   ; List utilities
             (srfi srfi-11)  ; let-values
             (ice-9 match))  ; Pattern matching

;;; Tower of Hanoi Simulator
(define (make-hanoi-state n)
  "Create initial Tower of Hanoi state with n disks on peg 0"
  (list (iota n n -1) '() '()))

(define (hanoi-valid-move? state disk from to)
  "Check if a move is valid in Tower of Hanoi"
  (let ((pegs (list->vector state)))
    (and (>= from 0) (< from 3)
         (>= to 0) (< to 3)
         (not (= from to))
         (not (null? (vector-ref pegs from)))
         (= disk (car (vector-ref pegs from)))
         (or (null? (vector-ref pegs to))
             (< disk (car (vector-ref pegs to)))))))

(define (hanoi-apply-move state disk from to)
  "Apply a move to Tower of Hanoi state"
  (let ((pegs (list->vector (map list-copy state))))
    (vector-set! pegs from (cdr (vector-ref pegs from)))
    (vector-set! pegs to (cons disk (vector-ref pegs to)))
    (vector->list pegs)))

(define (hanoi-goal-state? state n)
  "Check if state is the goal state"
  (equal? state (list '() '() (iota n n -1))))

(define (hanoi-solve state moves)
  "Validate a sequence of moves for Tower of Hanoi"
  (let loop ((current-state state)
             (remaining-moves moves)
             (move-count 0))
    (cond
     ((null? remaining-moves)
      (values current-state move-count #t))
     (else
      (match (car remaining-moves)
        ((disk from to)
         (if (hanoi-valid-move? current-state disk from to)
             (loop (hanoi-apply-move current-state disk from to)
                   (cdr remaining-moves)
                   (+ move-count 1))
             (values current-state move-count #f)))
        (_ (values current-state move-count #f)))))))

;;; Checker Jumping Simulator
(define (make-checker-state n)
  "Create initial Checker Jumping state with n red and n blue checkers"
  (append (make-list n 'R) '(_) (make-list n 'B)))

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

(define (checker-goal-state? board n)
  "Check if board is in goal state"
  (equal? board (append (make-list n 'B) '(_) (make-list n 'R))))

(define (checker-solve initial-board moves)
  "Validate a sequence of moves for Checker Jumping"
  (let loop ((board initial-board)
             (remaining-moves moves)
             (move-count 0))
    (cond
     ((null? remaining-moves)
      (values board move-count #t))
     (else
      (match (car remaining-moves)
        ((color from to)
         (if (checker-valid-move? board color from to)
             (loop (checker-apply-move board color from to)
                   (cdr remaining-moves)
                   (+ move-count 1))
             (values board move-count #f)))
        (_ (values board move-count #f)))))))

;;; River Crossing Simulator
(define (make-river-state n)
  "Create initial River Crossing state"
  `((left . ,(append (map (lambda (i) (string->symbol (format #f "a_~a" i))) (iota n 1))
                     (map (lambda (i) (string->symbol (format #f "A_~a" i))) (iota n 1))))
    (right . ())
    (boat . left)))

(define (river-safe? group)
  "Check if a group of people is safe (no actor without their agent with other agents)"
  (let* ((actors (filter (lambda (p) (char-lower-case? (string-ref (symbol->string p) 0))) group))
         (agents (filter (lambda (p) (char-upper-case? (string-ref (symbol->string p) 0))) group)))
    (every (lambda (actor)
             (let* ((actor-str (symbol->string actor))
                    (actor-num (substring actor-str 2))
                    (actor-agent (string->symbol (string-append "A_" actor-num))))
               (or (not (any (lambda (agent) (not (eq? agent actor-agent))) agents))
                   (member actor-agent group))))
           actors)))

(define (river-valid-move? state passengers capacity)
  "Check if a river crossing move is valid"
  (let ((boat-side (assq-ref state 'boat))
        (from-bank (assq-ref state boat-side)))
    (and (>= (length passengers) 1)
         (<= (length passengers) capacity)
         (every (lambda (p) (member p from-bank)) passengers)
         (river-safe? passengers)
         (river-safe? (lset-difference eq? from-bank passengers)))))

(define (river-apply-move state passengers)
  "Apply a river crossing move"
  (let* ((boat-side (assq-ref state 'boat))
         (other-side (if (eq? boat-side 'left) 'right 'left))
         (from-bank (assq-ref state boat-side))
         (to-bank (assq-ref state other-side)))
    `((left . ,(if (eq? boat-side 'left)
                   (lset-difference eq? from-bank passengers)
                   (append to-bank passengers)))
      (right . ,(if (eq? boat-side 'right)
                    (lset-difference eq? from-bank passengers)
                    (append to-bank passengers)))
      (boat . ,other-side))))

(define (river-goal-state? state n)
  "Check if everyone is on the right bank"
  (= (length (assq-ref state 'right)) (* 2 n)))

;;; Blocks World Simulator
(define (make-blocks-state n)
  "Create initial Blocks World state"
  (let ((blocks (map (lambda (i) (integer->char (+ i 65))) (iota n))))
    (list (list-head blocks (quotient n 2))
          (list-tail blocks (quotient n 2))
          '())))

(define (blocks-valid-move? state block from to)
  "Check if a blocks move is valid"
  (let ((stacks (list->vector state)))
    (and (>= from 0) (< from (vector-length stacks))
         (>= to 0) (< to (vector-length stacks))
         (not (= from to))
         (not (null? (vector-ref stacks from)))
         (eq? (car (vector-ref stacks from)) block))))

(define (blocks-apply-move state block from to)
  "Apply a move to blocks world state"
  (let ((stacks (list->vector (map list-copy state))))
    (vector-set! stacks from (cdr (vector-ref stacks from)))
    (vector-set! stacks to (cons block (vector-ref stacks to)))
    (vector->list stacks)))

;;; Example usage and tests
(define (test-hanoi)
  (let* ((n 3)
         (initial (make-hanoi-state n))
         (moves '((1 0 2) (2 0 1) (1 2 1) (3 0 2) (1 1 0) (2 1 2) (1 0 2))))
    (let-values (((final move-count valid?) (hanoi-solve initial moves)))
      (format #t "Tower of Hanoi (n=~a):~%" n)
      (format #t "  Initial: ~a~%" initial)
      (format #t "  Final: ~a~%" final)
      (format #t "  Valid: ~a~%" valid?)
      (format #t "  Goal reached: ~a~%~%" (hanoi-goal-state? final n)))))

(define (test-checker)
  (let* ((n 2)
         (initial (make-checker-state n))
         (moves '((R 1 2) (B 3 1) (B 4 3) (R 0 4) (R 2 0))))
    (let-values (((final move-count valid?) (checker-solve initial moves)))
      (format #t "Checker Jumping (n=~a):~%" n)
      (format #t "  Initial: ~a~%" initial)
      (format #t "  Final: ~a~%" final)
      (format #t "  Valid: ~a~%" valid?)
      (format #t "  Goal reached: ~a~%~%" (checker-goal-state? final n)))))

;; Run tests
(test-hanoi)
(test-checker)

;; Example: Generate moves for Tower of Hanoi recursively
(define (hanoi-generate-moves n source target auxiliary)
  "Generate optimal moves for Tower of Hanoi"
  (if (= n 1)
      (list (list n source target))
      (append (hanoi-generate-moves (- n 1) source auxiliary target)
              (list (list n source target))
              (hanoi-generate-moves (- n 1) auxiliary target source))))

(format #t "Optimal Tower of Hanoi solution for n=3:~%")
(format #t "~a~%" (hanoi-generate-moves 3 0 2 1))
