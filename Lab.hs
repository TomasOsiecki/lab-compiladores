{-# LANGUAGE GADTs #-}

module Lab (Expr(..), Ω, eInicial, eIniTest, eval) where

eInicial, eIniTest :: Σ
eInicial = \v -> undefined
eIniTest = \v -> 0

--         ∞
-- fix f = ⊔ fⁱ ⊥
--        i=0
fix :: (a -> a) -> a
fix f = f (fix f) -- A kind of magic!

type Iden = String

type Σ = Iden -> Int

-- Alias por si escribir Σ les resulta complicado
type State = Σ

-- Función de actualización de estado
update :: Σ -> Iden -> Int -> Σ
update σ v n v' =
  if v == v'
    then n
    else σ v'

{- Ω ≈ Σ + Σ + Z x Ω + Z → Ω -}
data Ω
  = Normal Σ
  | Abort Σ
  | Out (Int, Ω)
  | In (Int -> Ω)

{- Notar:
   * Normal : Σ → Ω
   * Abort  : Σ → Ω
   Extendemos con In y Out para manejar entradas y salidas
   * Out    : (Z, Ω) → Ω
   * In     : (Z → Ω) → Ω
   -}

-- Alias por si escribir Ω les resulta complicado
type Omega = Ω

data Expr a where
  {- Expresiones enteras -}

  -- n
  Const :: Int                  -> Expr Int
  -- v
  Var   :: Iden                 -> Expr Int
  -- e + e'
  Plus  :: Expr Int -> Expr Int -> Expr Int
  -- e - e'
  Dif   :: Expr Int -> Expr Int -> Expr Int
  -- e * e'
  Times :: Expr Int -> Expr Int -> Expr Int
  -- e / e' (división entera)
  Div   :: Expr Int -> Expr Int -> Expr Int
  -- Si e' evalúa a 0, hagan lo que quieran.

  {- Expresiones booleanas -}

  -- e = e'
  Eq   :: Expr Int  -> Expr Int -> Expr Bool
  -- e /= e'
  Neq  :: Expr Int  -> Expr Int -> Expr Bool
  -- e < e'
  Less :: Expr Int  -> Expr Int -> Expr Bool
  -- b && b'
  And  :: Expr Bool -> Expr Bool -> Expr Bool
  -- b || b'
  Or   :: Expr Bool -> Expr Bool -> Expr Bool
  -- ¬b
  Not  :: Expr Bool              -> Expr Bool

  {- Comandos -}

  -- SKIP
  Skip   ::                                    Expr Ω
  -- NEWVAR v := e IN c
  Local  :: Iden      -> Expr Int -> Expr Ω -> Expr Ω
  -- v := e
  Assign :: Iden      -> Expr Int           -> Expr Ω
  -- FAIL
  Fail   ::                                    Expr Ω
  -- CATCHIN c WITH c'
  Catch  :: Expr Ω    -> Expr Ω             -> Expr Ω
  -- WHILE b DO c
  While  :: Expr Bool -> Expr Ω             -> Expr Ω
  -- IF b THEN c ELSE c'
  If     :: Expr Bool -> Expr Ω   -> Expr Ω -> Expr Ω
  -- c ; c'
  Seq    :: Expr Ω    -> Expr Ω             -> Expr Ω
  -- OUTPUT (n, ω))
  Output :: Iden                            -> Expr Ω
  -- INPUT (Int -> Ω)
  Input  :: Iden                            -> Expr Ω


{- Completar las ecuaciones semánticas -}

class DomSem dom where
  sem :: Expr dom -> Σ -> dom

instance DomSem Int where
  sem (Const a) _    = a
  sem (Var v) σ      = σ v
  sem (Plus e1 e2) σ = sem e1 σ + sem e2 σ
  sem (Dif e1 e2) σ  = sem e1 σ - sem e2 σ
  sem (Times e1 e2) σ = sem e1 σ * sem e2 σ
  sem (Div e1 e2) σ   = 
    case sem e2 σ of
        0 -> error "Division by zero"
        _ -> sem e1 σ `div` sem e2 σ

instance DomSem Bool where
  sem (Eq e1 e2) σ = sem e1 σ == sem e2 σ
  sem (Neq e1 e2) σ = sem e1 σ /= sem e2 σ
  sem (Less e1 e2) σ = sem e1 σ < sem e2 σ
  sem (And e1 e2) σ = sem e1 σ && sem e2 σ
  sem (Or e1 e2) σ = sem e1 σ || sem e2 σ
  sem (Not e1) σ = not (sem e1 σ)

instance DomSem Ω where
  sem Skip σ = Normal σ
  sem (Local v e c) σ = rest †. sem c σ'
    where
        σ'   = update σ v (sem e σ)
        rest = \σ' -> update σ' v (σ v)
  sem (Assign v e) σ  = Normal (update σ v (sem e σ))
  sem Fail σ          = Abort σ
  sem (Catch c1 c2) σ = (sem c2) +. (sem c1 σ)
  sem (Seq c1 c2) σ   = (sem c2) *. (sem c1 σ)
  sem (While b c) σ   = fix f σ
    where 
      f w σ = if (sem b σ)
              then w *. (sem c σ)
              else Normal σ
  sem (If b c1 c2) σ  = if (sem b σ)
                        then sem c1 σ
                        else sem c2 σ
  sem (Output v) σ    = Out(sem (Var v) σ, Normal σ)
  sem (Input v) σ     = In (\n -> Normal (update σ v n))


{- Función de control para Ω -}

(*.) :: (Σ -> Ω) -> Ω -> Ω
(*.) f (Normal σ)   = f σ
(*.) _ (Abort σ)    = Abort σ
(*.) f (Out (n, ω)) = Out (n, f *. ω)
(*.) f (In g)       = In ((f *.) . g)

(+.) :: (Σ -> Ω) -> Ω -> Ω
(+.) _ (Normal σ)   = Normal σ
(+.) f (Abort σ)    = f σ
(+.) f (Out (n, ω)) = Out (n, f +. ω)
(+.) f (In g)       = In ((f +.) . g)

(†.) :: (Σ -> Σ) -> Ω -> Ω
(†.) f (Normal σ)   = Normal $ f σ
(†.) f (Abort σ)    = Abort $ f σ
(†.) f (Out (n, ω)) = Out (n, f †. ω)
(†.) f (In g)       = In ((f †.) . g)

{- ################# Funciones de evaluación de dom ################# -}

class Eval dom where 
  eval :: Expr dom -> Σ -> IO ()

instance Eval Int where
  eval e = print . sem e

instance Eval Bool where
  eval e = print . sem e

instance Eval Ω where
  eval e = unrollOmega . sem e
    where unrollOmega :: Ω -> IO ()
          unrollOmega (Normal σ)   = return ()
          unrollOmega (Abort σ)    = putStrLn "Abort"
          unrollOmega (Out (n, ω)) = print n >> unrollOmega ω
          unrollOmega (In f)       = getLine >>= unrollOmega . f . read
