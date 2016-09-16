local
  open Syntax
  open Result
  open TypeEnv
in
  signature TYPECHECKER =
  sig
    (* ρ; Γ ⊢ A. Check that A is a correct type expression. *)
    val checkT : int -> rho -> gamma -> exp  -> unit result

    (* ρ; Γ |- M ⇐ t.  Check that M is a well-typed expression of type t. *)
    val check  : int -> rho -> gamma -> exp  -> value -> unit result

    (* ρ; Γ ⊢ M ⇒ t. Infers the type of M. *)
    val checkI : int -> rho -> gamma -> exp -> value result

    (* ρ; Γ |- D  ⇒ Γ'. Checks that D is a valid declaration and
     * extends Γ to Γ'. *)
    val checkD : int -> rho -> gamma -> decl -> gamma result
  end
end
