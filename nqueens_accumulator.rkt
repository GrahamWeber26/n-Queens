;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nqueens_accumulator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; nqueens with accumulator
; CS 208 Program Design
; Dr. Derrick Tate
; Graham Weber with help from Jonathan Bergen and June
; December 6, 2021

(require 2htdp/abstraction)
(require htdp/show-queen)

; HELPER FUNCTIONS

; lst is a list
; list-of Items -> Item
; returns the last item in a list
(define (last lst)
  (first (reverse lst)))

; threatened?
; x, y are numbers; loq is a [list-of posns]
; Numbers, [list-of posns] -> boolean
; checks if the current position is threatened by any queen in loq
(define (threatened? x y loq)
  (cond
    [(empty? loq) #false]
    [(or
      (= x (posn-x (first loq)))
      (= y (posn-y (first loq)))
      (= (abs (- x (posn-x (first loq)))) (abs  (- y (posn-y (first loq)))))
      (threatened? x y (rest loq))) #true]
    [else #false]))

; showQueens
; loq is a list-of Booleans
; list-of Booleans -> image
; converts output into list suitable for show-queen
(define (showQueens n loq)
  (show-queen
   (for/list ([j (in-range n)])
    (for/list ([i (in-range n)])
      (if (member (make-posn i j) loq) #t #f)))))

; createBoard
; n is a Number
; Number -> [list-of posns]
; creates a blank board
(define (createBoard n)
  (for/list ([j (in-range n)] [i (in-range n)])
    (make-posn i j)))

; available?
(define (NA? x y board)
  (not (member? (make-posn x y) board)))

; place-queen
; x, y are numbers; loq is a [list-of posns]
; Numbers, [list-of posns] -> [list-of posns]
; appends a new posn with x and y to list of queens
(define (place-queen x y loq)
  (append loq (list (make-posn x y))))

; MAIN FUNCTIONS

; findQueensAcc
; n is a Number
; Number -> [list-of Posns]
; places n queens on an n by n board, else returns #false
(define (findQueensAcc n)
  (local (; n is a number, y is a number,
          ; n, y, loq, board -> loq OR #false
          ; places n queens on an n by n board, else returns #false
          (define (findQueensAcc-local n0 x y loq board)
            (cond
              [(= y n0) (showQueens n0 loq)] ; all queens placed, return list
              [(and (= x n0) (= y 0)) #false]  ; if first queen is past end of the board, all spots have been tried and there is no solution
              [else
               (cond
                 [(>= x n0) (findQueensAcc-local n0 (+ 1 (posn-x (last loq))) (- y 1) (remove (last loq) loq) (refreshBoard (remove (last loq) loq) board))] ; last col tried. Remove last q, move last q right 1
                  [(threatened? x y loq) (findQueensAcc-local n0 (+ x 1) y loq (refreshBoard (remove (last loq) loq) board))]        ; posn threatened. Move last q right 1
                 [else (findQueensAcc-local n0 0 (+ y 1) (place-queen x y loq) board)])]))) ; posn available. Place q, move to next row
    (findQueensAcc-local n 0 0 '() (createBoard n))))


; refreshBoard
(define (refreshBoard loq board) board)

(findQueensAcc 10)