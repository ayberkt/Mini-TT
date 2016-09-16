structure S = Syntax
structure R = Result

signature TYPE_ENV =
sig
  type gamma
  val lookupG : S.name -> gamma -> S.value
  val upG : gamma -> S.patt -> S.value -> S.value -> gamma R.result
end
