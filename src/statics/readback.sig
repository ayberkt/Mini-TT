local
  open Syntax
  open NormalForms
in
  signature READBACK =
  sig
    val readbackValue : int -> value -> nexp
    val readbackN : int -> neut -> nneut
    val readbackRho : int -> rho -> nrho
  end
end
