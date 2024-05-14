; Note to self:
; 1- Believe in yourself
; 2- Trust the Software Design Process
; 3- IF you think you need to finish this uninterruptedly or else you will lose your train of thoughts?
;    THEN that's bad code right there. (maybe I can make it a test for a good design)
;    A good code has enough documentation and modularity, that you can continue developing it after any interruption!
;    Yes, You need to catch up by reading a few lines, but it won't be painful.
;    You will appreciate a good design whenever you read it...



(require racket/list);gets list-ref, take and drop
;; sudoku-starter.rkt

;; Brute force Sudoku solver
;;
;; In Sudoku, the board is a 9x9 grid of SQUARES.
;; There are 9 ROWS and 9 COLUMNS, there are also 9
;; 3x3 BOXES.  Rows, columns and boxes are all UNITs.
;; So there are 27 units.
;;
;; The idea of the game is to fill each square with
;; a Natural[1, 9] such that no unit contains a duplicate
;; number.
;;

;; =================
;; Data definitions:


;; Val is Natural[1, 9]

;; Board is (listof Val|false)   that is 81 elements long
;; interp.
;;  Visually a board is a 9x9 array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

;; Pos is Natural[0, 80]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)     ;Natural[0, 8]
;;    - the column is (remainder p 9)    ;Natural[0, 8]


;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 9) c))  ;helpful for writing tests


;; Unit is (listof Pos) of length 9
;; interp. 
;;  The position of every square in a unit. There are
;;  27 of these for the 9 rows, 9 columns and 9 boxes.


;; =================
;; Constants:

(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))

(define B false) ;B stands for blank


(define BD1 
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD2 
  (list 1 2 3 4 5 6 7 8 9 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD3 
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))

(define BD4                ;easy
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))

(define BD4s               ;solution to 4
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))

(define BD5                ;hard
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B 
        B B B B B 6 B B B 
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))


(define BD5s               ;solution to 5
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))

(define BD6                ;hardest ever? (Dr Arto Inkala)
  (list B B 5 3 B B B B B 
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B 
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))

; Turned out not to be impossible for my tool to solve it :)
; (define BD6s
;   (list 1 4 5 3 2 7 6 9 8
;         8 3 9 6 5 4 1 2 7
;         6 7 2 9 1 8 5 4 3
;         4 9 6 1 8 5 3 7 2
;         2 1 8 4 7 3 9 5 6
;         7 5 3 2 9 6 4 8 1
;         3 6 7 5 4 2 8 1 9
;         9 8 4 7 6 1 2 3 5
;         5 2 1 8 3 9 7 6 4))


(define BD7                 ; no solution 
  (list 1 2 3 4 5 6 7 8 B 
        B B B B B B B B 2 
        B B B B B B B B 3 
        B B B B B B B B 4 
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))




;; Positions of all the rows, columns and boxes:

(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))

(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))

(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

(define UNITS (append ROWS COLS BOXES))




;; =================
;; Functions:




;; Board Pos -> Val or false
;; Produce value at given position on board.
(check-expect (read-square BD2 (r-c->pos 0 5)) 6)
(check-expect (read-square BD3 (r-c->pos 7 0)) 8)

(define (read-square bd p)
  (list-ref bd p))               


;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill-square BD1 (r-c->pos 0 0) 1)
              (cons 1 (rest BD1)))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))


; 
; We could have coded read-square and fill-square 'from scratch'
; by using the functions operating on 2 one-of data rule. If we 
; had, the function definitions would look like this:
; 
; 
; ; 
; ; Function on 2 complex data: Board and Pos.
; ; We can assume that p is <= (length bd).
; ; 
; ;               empty     (cons Val-or-False Board)
; ;  0             XXX         (first bd)
; ;  
; ;  (add1 p)      XXX         <natural recursion>
; 
; 
; (define (read-square bd p)  
;   (cond [(zero? p) (first bd)]
;         [else
;          (read-square (rest bd) (sub1 p))]))
; 
; 
; 
; ; 
; ; Function on 2 complex data, Board and Pos.
; ; We can assume that p is <= (length bd).
; ; 
; ;               empty     (cons Val-or-False Board)
; ;  0             XXX         (cons nv (rest bd))
; ;  
; ;  (add1 p)      XXX         (cons (first bd) <natural recursion>)
;  
; (define (fill-square bd p nv)  
;   (cond [(zero? p) (cons nv (rest bd))]
;         [else
;          (cons (first bd)
;                (fill-square (rest bd) (sub1 p) nv))]))
; 



;; Board -> Board or false
;; produce the answer to the bd or false if unsolveable
(check-expect (solve BD4) BD4s)   
(check-expect (solve BD5) BD5s)
(check-expect (solve BD7) false)
; (define (solve bd) false) ;stub

(define (solve bd)
  (local [(define (solve--bd bd)
            (if (solved? bd)
                bd
                (solve--lobd(next-boards bd))))

          (define (solve--lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (solve--bd(first lobd)))]
                     (if (false? try)
                         (solve--lobd (rest lobd))
                         try  ))]))]
    (solve--bd bd)))



;; Board -> Boolean
;; produce true if the board is solved
;; ASSUME: Board is valid, so if all of the squares are numbers, it is solved
(check-expect (solved? BD1) false)
(check-expect (solved? BD4) false)
(check-expect (solved? BD4s) true)
(check-expect (solved?
               (list 5 3 9 1 6 4 8 7 2
                     8 1 2 7 5 3 6 9 4
                     6 7 4 9 2 8 3 1 5
                     2 9 6 4 1 7 5 8 3
                     1 8 7 2 3 5 9 4 6
                     3 4 5 8 B 6 1 2 7
                     9 2 3 5 4 1 7 6 8
                     7 6 1 3 8 2 4 5 9
                     4 5 8 6 7 9 2 3 1)) false)

; (define (solved? bd) false) ;stub
(define (solved? bd)
  (andmap number? bd))


;; Board -> (Listof Board)
;; produce a list of boards
;; find the next blank, fill the next blank will Natural[1, 9], filter out the invalid boards
(check-expect (next-boards BD1)                 
              (list (cons 1 (rest BD1))
                    (cons 2 (rest BD1))
                    (cons 3 (rest BD1))
                    (cons 4 (rest BD1))
                    (cons 5 (rest BD1))
                    (cons 6 (rest BD1))
                    (cons 7 (rest BD1))
                    (cons 8 (rest BD1))
                    (cons 9 (rest BD1))))

(check-expect (next-boards
               (list 5 3 9 1 6 4 8 7 2
                     8 1 2 7 5 3 6 9 4
                     6 7 4 9 2 8 3 1 5
                     2 9 6 4 1 7 5 8 3
                     1 8 7 2 3 5 9 4 6
                     3 4 5 8 B 6 1 2 7
                     9 2 3 5 4 1 7 6 8
                     7 6 1 3 8 2 4 5 9
                     4 5 8 6 7 9 2 3 1))
              (list BD5s))

; (define (next-boards bd) empty) ;stub

(define (next-boards bd)
  (local [(define pos (find-blank bd))
          (define (valid? bd) (valid-board? bd pos))]
    (filter valid? (fill-blank-1-9 bd pos))))


;; Board -> Pos
;; produce the next blank position in the board
;; ASSUME board is not full
(check-expect (find-blank BD1) 0)
(check-expect (find-blank (cons 1 (rest BD1))) 1)
(check-expect (find-blank (cons 1 (cons 2 (rest (rest BD1))))) 2)
; (define (find-blank bd) 0) ;stub
(define (find-blank bd)
  (cond [(empty? bd) (error "board wasn't supposed to be full")]
        [else
         (if (false? (first bd))
             0
             (add1 (find-blank(rest bd))))]))


;; Board Pos -> (ListOf Board)
;; produce a list of new boards with position p filled with Natural[1, 9]
(check-expect (fill-blank-1-9 BD1 0)
              (list (cons 1 (rest BD1))
                    (cons 2 (rest BD1))
                    (cons 3 (rest BD1))
                    (cons 4 (rest BD1))
                    (cons 5 (rest BD1))
                    (cons 6 (rest BD1))
                    (cons 7 (rest BD1))
                    (cons 8 (rest BD1))
                    (cons 9 (rest BD1))))
(check-expect (fill-blank-1-9 (cons 1 (rest BD1)) 1)
              (list (cons 1 (cons 1 (rest (rest BD1))))
                    (cons 1 (cons 2 (rest (rest BD1))))
                    (cons 1 (cons 3 (rest (rest BD1))))
                    (cons 1 (cons 4 (rest (rest BD1))))
                    (cons 1 (cons 5 (rest (rest BD1))))
                    (cons 1 (cons 6 (rest (rest BD1))))
                    (cons 1 (cons 7 (rest (rest BD1))))
                    (cons 1 (cons 8 (rest (rest BD1))))
                    (cons 1 (cons 9 (rest (rest BD1))))))
; (define (fill-blank-1-9 bd p) empty) ;stub

(define (fill-blank-1-9 bd p)
  (local [(define (fn n) (fill-square bd p (add1 n)))]
    (build-list 9 fn)))



;; Board -> Boolean
;; produce true if the board is valid with regard to position p (only check col, row and box in which position p exists)
;; based on soduko rules: no duplicate number in any row, column or box
;; find units where position p exists, fill position with values from bd, get rid of blanks(false) , if there is duplicate number, answer is false
; (define (valid-board? bd p) false) ;stub

(check-expect (valid-board?               
               (list B B B B B B B B B
                     B B B B B B B B B
                     B B B B B B B B B
                     B B B B B B B B B
                     B B B B B B B B B
                     B B B B B B B B B
                     B B B B B B B B B
                     B B B B B B B B B
                     B B B B B B B B B) 0) true)
(check-expect (valid-board? BD5 29) true);; true
(check-expect (valid-board?
               (list 1 B B B 1 B 5 B B
                     B B B B B B B B B
                     B B B B B B 8 B B
                     B B B B B B B B B
                     4 B B B B B B B B
                     B B B B B B B B B
                     B B B B B B 6 B B
                     B B B B B B B B B
                     B B B B B B B B B) 0) false) ;; failed, row
(check-expect (valid-board?
               (list 1 B B B 8 B 5 B B
                     B B B B B B B B B
                     B B B B B B 8 B B
                     B B B B B B B B B
                     1 B B B B B B B B
                     B B B B B B B B B
                     B B B B B B 6 B B
                     B B B B B B B B B
                     B B B B B B B B B) 0) false) ;; failed, col
(check-expect (valid-board?
               (list 1 B B B 8 B 5 B B
                     B B B B B B B B B
                     B B 1 B B B 8 B B
                     B B B B B B B B B
                     9 B B B B B B B B
                     B B B B B B B B B
                     B B B B B B 6 B B
                     B B B B B B B B B
                     B B B B B B B B B) 0) false) ;; failed, box


(define (valid-board? bd p)
  (local [(define (filter-unit u) (filter number? u))]
  (andmap valid-unit? (map filter-unit (fill-units bd (find-units p))))))


;; Pos -> (listof Unit)
;; produce a list of units (col,row or box) where pos is within them
(check-expect (find-units  0) (list (list  0  1  2  3  4  5  6  7  8) (list 0  9 18 27 36 45 54 63 72) (list  0  1  2  9 10 11 18 19 20)))
(check-expect (find-units 55) (list (list 54 55 56 57 58 59 60 61 62) (list 1 10 19 28 37 46 55 64 73) (list 54 55 56 63 64 65 72 73 74)))

; (define (find-units p) empty) ;stub

(define (find-units p)
  (local [(define (has-p? u)
            (cond [(empty? u) false]
                  [else (or (= (first u) p)
                            (has-p? (rest u)))]))]
    (filter has-p? UNITS)))


;; Board (Listof Unit) -> (Listof Unit)
;; fill the list of units by their values from the bd
(check-expect (fill-units BD1 (list (list 1 2 3))) (list (list B B B)))
(check-expect (fill-units BD2 (list (list 0 1 2 3))) (list(list 1 2 3 4)))
(check-expect (fill-units BD2 (list (list 0 1 2 3) (list 8 9 10))) (list(list 1 2 3 4) (list 9 B B)))
; (define (fill-units bd lou) empty) ;stub
(define (fill-units bd lou)
  (local [(define (fill-pos p)
            (read-square bd p))
          (define (fill-unit u)
            (map fill-pos u))]
    (map fill-unit lou)))


;; Unit -> Boolean
;; produce true if there is no duplicate number in the unit
;; ASSUME: all elements are numbers
(check-expect (valid-unit? empty) true)
(check-expect (valid-unit? (list  1  2  3  4  5  6  7  8)) true)
(check-expect (valid-unit? (list  1  1  2  3  4  5  6  7  8)) false)
(check-expect (valid-unit? (list  1  2  3  4  5  6  2  8)) false)
; (define (valid-unit? u) false) ;stub

(define (valid-unit? u)
  (local [(define (has? p u)
            (cond [(empty? u) false]
                  [else (if (= p (first u))
                            true
                            (has? p (rest u)))]))]
    (cond [(empty? u) true]
          [else
           (if (has? (first u) (rest u))
               false
               (valid-unit?(rest u)))])))




