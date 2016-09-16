structure S = Syntax

signature MONAD =
sig
  type 'a result

  val >>= : ('a result * ('a -> 'b result)) -> 'b result

  val return : 'a -> 'a result
  val fail   : S.name -> 'a result
end

structure Result : MONAD =
struct
  datatype 'a result = SUCCESS of 'a | FAIL of S.name

  fun >>= (SUCCESS x, k) = k x
    | >>= (FAIL s, _) = FAIL s

  val return = fn x => SUCCESS x
  val fail = fn x => FAIL x
end

structure Context =
  SplayMapFn(
      struct
        type ord_key = S.name
        val compare = String.compare
      end
  )

local
  open Result
  infix 2 >>=
  infix 5 **
  open Context
  open Syntax
in
  type gamma = S.value map

  fun lookupG s gma = lookup (gma, s)

  fun upG gma PUNIT _ _ = return gma
    | upG gma (PVAR x) t _ = return ((x, t)::gma)
    | upG gma (PPAIR (p1, p2)) (SIGMA (t, g)) v =
        upG gma p1 t (vfst v) >>=
        (fn gma1 => upG gma1 p2 (g ** vfst v) (vsnd v))
    | upG _ p _ _ = raise Fail "problem in upG"
end
