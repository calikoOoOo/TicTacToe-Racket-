;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tictactoe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;; A Status is one of: 'X 'O '_
;; A Player is one of: 'X 'O

;; A Tic-Tac-Toe Grid (T3Grid) is a (listof (listof (anyof 'X 'O '_)))
;; Requires: all lists have the same length, and that length is odd
;; The number of 'X and 'O is equal, or there is one more 'X

(define counter-starter 0)

;; question a)

;; (whose-turn grid) consumes a v and says whose turn it is,
;; knowing X goes first
;; Examples:

(check-expect (whose-turn (list (list '_ '_ '_)
                                (list '_ '_ '_)
                                (list '_ '_ '_))) 'X)
(check-expect (whose-turn (list (list 'X '_ '_)
                                (list '_ 'O '_)
                                (list '_ '_ 'X))) 'O)
(check-expect (whose-turn (list (list 'X 'O '_)
                                (list '_ '_ '_)
                                (list '_ '_ '_))) 'X)

;; whose-turn: T3Grid -> (Anyof Player false)
(define (whose-turn grid)
  (cond [(= (num-of-plays-tot grid counter-starter 'X) (num-of-plays-tot grid counter-starter 'O)) 'X]
        [else 'O]))


;; Helper functions:

;; (num-of-plays-tot grid counter player) consumes a T3Grid counter and Player
;; and produces the total amount of player marks on the grid
;; Examples:

(check-expect (num-of-plays-tot (list (list 'X 'O '_)
                                (list '_ '_ '_)
                                (list '_ '_ '_)) 0 'X) 1)
(check-expect (num-of-plays-tot (list (list '_ '_ '_ '_ '_ '_ '_)
                                (list '_ '_ '_ '_ 'O 'O '_)
                                (list '_ 'X 'X '_ '_ '_ '_)
                                (list '_ '_ '_ '_ '_ '_ '_)
                                (list '_ '_ '_ 'X '_ '_ '_)
                                (list '_ 'O '_ '_ '_ '_ 'X)
                                (list '_ '_ '_ '_ '_ '_ '_)) 0 'O) 3)
(check-expect (num-of-plays-tot empty 0 'X) 0)

;; num-of-plays-tot: T3Grid Nat Player -> Nat
(define (num-of-plays-tot grid counter player)
  (cond [(empty? grid) counter]
        [(list? (first grid))
         (num-of-plays-tot (rest grid)
                           (+ counter (num-of-plays (first grid) counter-starter player))
                           player)]))

;; (num-of-plays current-list counter player) consumes a part of a T3Grid, a counter and
;; Player to produce the amount of player marks in that part of the grid
;; Examples:

(check-expect (num-of-plays (list 'X 'O '_) 0 'X) 1)
(check-expect (num-of-plays empty 0 'X) 0)
(check-expect (num-of-plays (list '_ '_ '_ '_ '_ '_ '_) 0 'O) 0)

;; num-of-plays: (listof Sym) Nat Player -> Nat
(define (num-of-plays current-list counter player)
  (cond [(empty? current-list) counter]
        [(symbol=? player (first current-list))
         (num-of-plays (rest current-list) (add1 counter) player)]
        [else (num-of-plays (rest current-list) counter player)]))

;; question b)

;; (grid-ref grid row column) consumes a T3Grid [grid], [row] number,
;; and [column] number to produce the Symbol located at that location
;; Examples:

(check-expect (grid-ref (list (list 'X)) 0 0) 'X)
(check-expect (grid-ref (list (list '_ '_ '_)
                              (list '_ '_ '_)
                              (list '_ '_ '_)) 0 2) '_)
(check-expect (grid-ref (list (list '_ '_ 'O)
                              (list 'X '_ '_)
                              (list '_ '_ '_)) 1 0) 'X)

;; grid-ref: T3Grid Nat Nat -> (Anyof Status false)
;; Requires: row and column must be within the bounds of the grid

(define (grid-ref grid row column)
  (cond [(zero? row) (list-ref (first grid) column)]
        [else (grid-ref (rest grid) (sub1 row) column)]))

;; Tests:
(check-expect (grid-ref (list (list '_ '_ 'O)
                              (list 'X '_ '_)
                              (list '_ '_ '_)) 2 2) '_)

;; question c)

;; (get-column grid column) consumes a T3Grid and a column number
;; to produce a list of symbols in that column
;; Examples:

(check-expect (get-column (list (list '_ 'O '_)
                                (list 'X '_ '_)
                                (list '_ '_ '_)) 1) (list 'O '_ '_))
(check-expect (get-column (list (list 'X 'X 'X)
                                (list 'O 'O '_)
                                (list '_ 'O '_)) 2) (list 'X '_ '_))
(check-expect (get-column (list (list '_ '_ '_ '_ '_ '_ '_)
                                (list '_ '_ '_ '_ 'O 'O '_)
                                (list '_ 'X 'X '_ '_ '_ '_)
                                (list '_ '_ '_ '_ '_ '_ '_)
                                (list '_ '_ '_ 'X '_ '_ '_)
                                (list '_ 'O '_ '_ '_ '_ 'X)
                                (list '_ '_ '_ '_ '_ '_ '_)) 5)
              (list '_ 'O '_ '_ '_ '_ '_))

;; get-column: T3Grid Nat -> (listof Status)
(define (get-column grid column)
  (cond [(empty? grid) empty]
        [else (cons (list-ref (first grid) column)
                    (get-column (rest grid) column))]))

;; Tests:
(check-expect (get-column empty 2) empty)


;; (question d)

;; (will-win? grid row column player) consumes a T3Grid, a row number,
;; a column number, and a player Symbol ('X or 'O) and produce true if
;; that player will win when placing a marker on the given row,
;; column location, and produce false otherwise
;; Examples:

(check-expect (will-win? (list (list 'X '_ 'X)
                               (list '_ 'O '_)
                               (list 'O '_ '_)) 0 1 'X) true)
(check-expect (will-win? (list (list 'X 'X 'O)
                               (list '_ 'O '_)
                               (list 'O '_ '_)) 0 2 'X) false)
(check-expect (will-win? (list (list 'O '_ 'O)
                               (list '_ 'X '_)
                               (list '_ 'X 'X)) 1 2 'O) false)

;; will-win?: T3Grid Nat Nat Player -> Bool
(define (will-win? grid row column player)
  (cond [(not (symbol=? '_ (grid-ref grid row column))) false]
        [(and (symbol=? 'X player)
              (or (row/col-won? grid
                                (count-in-row grid row counter-starter player))
                  (row/col-won? grid
                                (count-in-col grid column
                                              counter-starter player)))) true]
        [(and (symbol=? 'O player)
              (or (row/col-won? grid
                                (count-in-row grid row counter-starter player))
                  (row/col-won? grid
                                (count-in-col grid column
                                              counter-starter player)))) true]
        [else false]))

;; Tests:
(check-expect (will-win? (list (list 'O '_ '_)
                               (list 'O 'X '_)
                               (list '_ 'X 'X)) 2 0 'O) true)
(check-expect (will-win? (list (list 'O '_ '_)
                               (list 'O 'X '_)
                               (list '_ 'X 'X)) 0 1 'X) true)

;; (row/col-won? grid num-in-row/col) consumes a T3Grid and the number of
;; a specified player in a row/column, to produce true if there is enough
;; to win, and false if there is not enough Player statuses to win
;; Examples:

(check-expect (row/col-won? (list (list 'O '_ '_)
                                  (list 'O 'X '_)
                                  (list '_ 'X 'X)) 2) true)
(check-expect (row/col-won? (list (list 'O '_ '_)
                                  (list 'O 'X '_)
                                  (list '_ 'X 'X)) 0) false)
(check-expect (row/col-won? empty 2) false)

;; row/col-won? T3Grid Nat -> Bool
(define (row/col-won? grid num-in-row/col)
  (cond [(= (- (length grid) 1) num-in-row/col) true]
        [else false]))

;; (count-in-row grid row counter player) counts how many Player markers
;; are on a specific row
;; Examples:

(check-expect (count-in-row (list (list 'O '_ '_)
                                  (list 'O 'X '_)
                                  (list '_ 'X 'X)) 2 0 'X) 2)
(check-expect (count-in-row (list (list 'O '_ '_)
                                  (list 'O 'X '_)
                                  (list '_ 'X 'X)) 2 0 'O) 0)
(check-expect (count-in-row empty 2 0 'X) 0)

;; count-in-row: T3Grid Nat Nat Player -> Nat
(define (count-in-row grid row counter player)
  (cond [(empty? grid) counter]
        [(zero? row) (num-of-plays (first grid) counter player)]
        [else (count-in-row (rest grid) (sub1 row) counter player)]))


;; (count-in-col grid column counter player) counts how many Player markers
;; are on a specific column
;; Examples:
(check-expect (count-in-col (list (list 'O '_ '_)
                                  (list 'O 'X '_)
                                  (list '_ 'X 'X)) 1 0 'X) 2)
(check-expect (count-in-col (list (list 'O '_ '_)
                                  (list 'O 'X '_)
                                  (list '_ 'X 'X)) 2 0 'O) 0)
(check-expect (count-in-col empty 2 0 'X) 0)

;; count-in-col: T3Grid Nat Nat Player -> Nat
(define (count-in-col grid column counter player)
  (cond [(empty? grid) counter]
        [else (num-of-plays (get-column grid column) counter player)]))
