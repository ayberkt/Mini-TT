structure Readback : READBACK =
struct
  open NormalForms
  open Syntax

  exception Todo

  fun readbackValue k v0 =
    case v0 of
        LAM f       => NLAM k
      | PAIR (u, v) => NPAIR (readbackValue k u, readbackValue k v)

  fun readbackRho x y = raise Todo

  fun readbackN x y = raise Todo
end
