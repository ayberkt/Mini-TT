structure Readback : READBACK =
struct
  open NormalForms
  open Syntax
  infixr 5 **

  exception Todo

  fun @@ (f, x) = f x
  infixr 1 @@

  (* This should probably be in some other module. *)
  fun genValue (x : int) : value = raise Todo

  fun readbackValue (k : int) (v0 : value) =
    case v0 of
        LAM f         => NLAM (k, readbackValue (k + 1) @@ f ** (genValue k))
      | (PAIR (u, v)) => NPAIR (readbackValue k u, readbackValue k v)

  fun readbackRho x y = raise Todo

  fun readbackN x y = raise Todo
end
