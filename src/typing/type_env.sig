local
  structure S = Syntax
  structure R = Result
in
  signature TYPE_ENV =
  sig
    exception TypeEnvError of string
    type gamma
    val lookupG : S.name -> gamma -> S.value option
    val upG : gamma -> S.patt -> S.value -> S.value -> gamma option
  end
end
