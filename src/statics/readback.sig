local
  open Syntax
  open NormalForms
in
  signature READBACK =
  sig
    val rbValue : int -> value -> nexp
    val rbNeut : int -> neut -> nneut
    val rbRho : int -> rho -> nrho
    val genValue : int -> Syntax.value
  end
end
