local
  open Syntax
  open Result
  open TypeEnv
in
  signature TYPECHECKER =
  sig
    val checkT : int -> rho -> gamma -> exp  -> unit result
    val check  : int -> rho -> gamma -> exp  -> value -> unit result
    val checkI : int -> rho -> gamma -> exp  -> value result
    val checkD : int -> rho -> gamma -> decl -> gamma result
  end
end
