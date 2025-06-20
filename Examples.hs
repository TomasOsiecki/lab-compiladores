{-# LANGUAGE GADTs #-}

module Examples where

import Lab (Expr(..), Ω, eInicial, eval)

-- Ejemplo 1: Asignación simple
progAssignSimple :: Expr Ω
progAssignSimple = Assign "x" (Const 10)

-- Ejemplo 2: Secuencia de asignaciones
progSeqAssign :: Expr Ω
progSeqAssign = Seq (Assign "x" (Const 5))
                   (Assign "y" (Plus (Var "x") (Const 2)))

-- Ejemplo 3: Condicional simple
progIfSimple :: Expr Ω
progIfSimple = If (Less (Const 3) (Const 5))
                   (Assign "x" (Const 1))
                   (Assign "x" (Const 0))

-- Ejemplo 4: While simple
progWhileSimple :: Expr Ω
progWhileSimple = Seq (Assign "x" (Const 0))
                     (While (Less (Var "x") (Const 3))
                            (Assign "x" (Plus (Var "x") (Const 1))))

-- Ejemplo 5: Output simple
progOutputSimple :: Expr Ω
progOutputSimple = Seq (Assign "x" (Const 42))
                      (Output "x")

-- Ejemplo 6: Input simple y Output
progInputOutputSimple :: Expr Ω
progInputOutputSimple = Seq (Input "n")
                           (Output "n")

-- Ejemplo 7: Loop con input y output (cuenta regresiva)
progLoopInputOutput :: Expr Ω
progLoopInputOutput = Seq (Input "n")
                         (While (Less (Const 0) (Var "n"))
                                (Seq (Output "n")
                                     (Assign "n" (Dif (Var "n") (Const 1)))))


-- Ejemplo 8: Fail sin catch (debe abortar)
progFailAbort :: Expr Ω
progFailAbort = Seq (Assign "x" (Const 1))
                   Fail

-- Ejemplo 9: While con if interno para sumar condicionalmente
progWhileIf :: Expr Ω
progWhileIf = Seq (Assign "x" (Const 0))
                 (While (Less (Var "x") (Const 5))
                    (If (Less (Var "x") (Const 3))
                        (Assign "x" (Plus (Var "x") (Const 2)))
                        (Assign "x" (Plus (Var "x") (Const 1)))))

-- Factorial: pide "n", calcula factorial y output "fact"
progFactorial :: Expr Ω
progFactorial = Seq
  (Input "n")
  (Seq
    (Assign "fact" (Const 1))
    (Seq
      (Assign "i" (Const 1))
      (Seq
        (While (Less (Var "i") (Plus (Var "n") (Const 1)))
          (Seq
            (Assign "fact" (Times (Var "fact") (Var "i")))
            (Assign "i" (Plus (Var "i") (Const 1)))
          )
        )
        (Output "fact")
      )
    )
  )

-- Fibonacci: pide "n", calcula fib(n) y output "fib"
progFibonacci :: Expr Ω
progFibonacci = Seq
  (Input "n")
  (If (Less (Var "n") (Const 2))
      (Seq
        (Assign "fib" (Const 1))
        (Output "fib")
      )
      (Seq
        (Assign "fib1" (Const 1))
        (Seq
          (Assign "fib2" (Const 1))
          (Seq
            (Assign "i" (Const 2))
            (Seq
              (While (Less (Var "i") (Plus (Var "n") (Const 1)))
                (Seq
                  (Assign "fib" (Plus (Var "fib1") (Var "fib2")))
                  (Seq
                    (Assign "fib2" (Var "fib1"))
                    (Seq
                      (Assign "fib1" (Var "fib"))
                      (Assign "i" (Plus (Var "i") (Const 1)))
                    )
                  )
                )
              )
              (Output "fib")
            )
          )
        )
      )
  )

-- MCD: pide "a" y "b", calcula mcd y output "a"
progMcd :: Expr Ω
progMcd = Seq
  (Input "a")
  (Seq
    (Input "b")
    (Seq
      (While (Neq (Var "b") (Const 0))
        (Seq
          (Assign "r" (Dif (Var "a") (Times (Div (Var "a") (Var "b")) (Var "b"))))
          (Seq
            (Assign "a" (Var "b"))
            (Assign "b" (Var "r"))
          )
        )
      )
      (Output "a")
    )
  )
