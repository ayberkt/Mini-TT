structure Context =
  SplayMapFn(
      struct
        type ord_key = S.name
        val compare = String.compare
      end
  )

structure TypeEnv : TYPE_ENV =
struct
  open Result
  infix 2 >>=
  infix 5 **
  open Context
  open Syntax

  type gamma = S.value map

  fun lookupG s gma = lookup (gma, s)

  fun upG gma PUNIT _ _ = return gma
    | upG gma (PVAR x) t _ = return (Context.insert (gma, x, t))
    | upG gma (PPAIR (p1, p2)) (SIGMA (t, g)) v =
        upG gma p1 t (vfst v) >>= (fn gma1 =>
        upG gma1 p2 (g ** vfst v) (vsnd v))
    | upG _ p _ _ = raise Fail "problem in upG"
end
