signature READBACK =
sig
  val readbackValue : int -> value -> nexp
  val readbackN : int -> neut -> nneut
end
