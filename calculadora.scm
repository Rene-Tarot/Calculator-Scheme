;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Estas son las variable globales ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define res 0)
(define check #f)
(define valor_1 #\s)
(define valor_2 #\s)
(define sub 0)
(define signo #\s)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Esta subrutina mira si el numero de parentesis abiertos sea igual que los cerrados ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parentesis input)
 (define contador 0)
 (define acc 0)
 
(define (ciclo-par)
  (if (< contador (string-length input))
      (if (equal? (string-ref input contador) #\()
            (begin
              (set! contador (+ contador 1))
              (set! acc (+ acc 1))
              (ciclo-par)
            )

            (begin
             (if (equal? (string-ref input contador) #\))
                (begin
                 (set! contador(+ contador 1))
                 (set! acc (- acc 1))
                 (ciclo-par)
                )
             (begin
               (set! contador (+ contador 1))
               (ciclo-par)
             )
            )
           )
       )
  )
)
(ciclo-par)

  (if (= acc 0)
      (set! check #t)
  )
  
  (if (not (equal? check #t))

    (begin
     (display "respuesta>> ") (display "Error! El numero de parentesis abiertos no es el mismo que los parentesis cerrados.")(newline)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Esta subrutina encuentra el primer parentesis abierto ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Parametros: string (Lo que ingresa el usuario), char (caracter a encontrar);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-first
   (lambda (x y)
     (let z
            ([a (string->list x)]
             [b 0])

             (if (null? a)

                 #f

                 (if (equal? (car a) y)

                     b

                     (z (cdr a) (+ b 1))
                 )
             )
      )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Esta subrutina encuentra el primer parentesis cerado ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Parametros: string (Lo que ingresa el usuario), char (caracter a encontrar);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-last
  (lambda (x y)
    (let z
           ([a (string->list x)]
            [b 0]
            [c -1])

           (if (null? a)

               (if (= -1 c)

                   #f

                   c
               )

               (if (equal? (car a) y)

                   (z (cdr a) (+ b 1) (+ b 0))

                   (z (cdr a) (+ b 1) (+ c 0))
               )
            )
      )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Esta subrutina hace un set para los valores numericos para las operaciones;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simples input)

  (define cerrado 0)
  (define espacio_1 0)
  (define espacio_2 0) 
 
  (set! sub (substring input (search-last input #\() (+ (search-first input #\) ) 1)))
  (set! espacio_1  (search-first input #\space ))
  (set! espacio_2  (search-last  input #\space ))
  (set! signo (substring sub 1  espacio_1 ))
  (set! cerrado (search-last input  #\))) 

  (if (or (equal? signo "+") (equal? signo "-") (equal? signo "*") (equal? signo "/") (equal? signo "%") (equal? signo "div"))

      (begin
        (set! valor_1 (substring sub  (+ espacio_1 1 )(+ espacio_2 ) ))
        (set! valor_2 (substring sub  (+ espacio_2 1) cerrado))
      )
  )

  (if (or (equal? signo "fact!") (equal? signo "fibonacci") (equal? signo "sqroot") (equal? signo "sqr") (equal? signo "sen") (equal? signo "cos") (equal? signo "tan"))

    (begin
      (set! valor_1 (substring sub  (+ espacio_1 1 ) cerrado  ))
      (set! valor_2 "0")
      )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Esta subrutina hace el factorial de un numero ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parametros: valor_1 (un numero) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factorial valor_1)

  (if (= valor_1 0)

       1

       (* valor_1 (factorial (- valor_1 1)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Esta subrutina hace el Fibonacci de un numero ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parametros: valor_1 (un numero) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fibonacci valor_1)


  
  (if (= valor_1 0)

       0

      (if (= valor_1 1)

           1

           (+ (fibonacci (- valor_1 1)) (fibonacci (- valor_1 2)))
      )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Esta subrutina realiza todas la operaciones aritmeticas de la calculadora;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Las operaciones son: suma, resta, multiplicación, división,...;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (operaciones   signo valor_1 valor_2)
 
  ( set! valor_1 (string->number valor_1))
  ( set! valor_2 (string->number valor_2))

  (if (equal? signo "+")
        (begin
         (set! res  (+ valor_1 valor_2))
         (set! res (number->string res))
        )  
  )
  
 (if (equal? signo "-")
       (begin
         (set! res  (- valor_1 valor_2 ))
         (set! res (number->string res ))
       )
 )   

  (if (equal? signo "*")
        (begin
          (set! res (* valor_1 valor_2 ))
          (set! res (number->string res ))
        )
  )

  (if (equal? signo "/") 

      (if (= valor_2 0)

          (set! res "ERROR! Divición entre cero")

          (begin
            (set! res (/ valor_1 valor_2 ))
            (set! res (number->string res ))
          )
      )
   )
      
    (if (equal? signo "sqr")

          (begin
            (set! res (expt valor_1 2 ))
            (set! res (number->string res ))
          )
    )

  (if  (equal? signo "sqrt")

         (begin
           (set! res (sqrt valor_1))
           (set! res (number->string res))
         )
  )
   
  (if (equal? signo "%")

       (if (negative? valor_2)

            (set! res "ERROR! Divición entre cero ")

            (begin
              (set! res (remainder valor_1 valor_2))
              (set! res (number->string res))
            )
       )
   )

  (if (equal? signo "div")

      (begin
        (set! res (residue valor_1 valor_2))
        (set! res (number->string res))
      )
  )

  (if (equal? signo "fact!")

      (set! res (factorial valor_1))
  )

  (if (equal? signo "fibonacci")

      (set! res (fibonacci valor_1))
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Este es el main del programa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main)

  (newline)(display "Bienvenidos a la calculadora Impropia")(newline)
  (display "Hecho por: René Tarot 15012265")(newline)
  (display "           Eswin Monroy 16007809")(newline)
  (display "Calculadora>>")
  (define input (read-line))
      
  (parentesis input)

  (if (equal? check #f)

      ((newline)(main))
  )

 (simples input)
 (operaciones signo valor_1 valor_2)
 (display "Respuesta>> ")(display res)
 (set! valor_1 #\s)
 (set! valor_2 #\s)
 (newline)(main)
)
(main)