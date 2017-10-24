local
  open Syntax
  open Result
  open TypeEnv
in
  signature TYPECHECKER =
  sig
    exception TypeError of string

    (* ρ; Γ ⊢ A. Check that A is a correct type expression. *)
    val validType : int -> rho -> gamma -> exp -> bool

    (* ρ; Γ |- M ⇐ t.  Check that M is a well-typed expression of type t. *)
    val check  : int -> rho -> gamma -> exp  -> value -> bool

    (* ρ; Γ ⊢ M ⇒ t. Infers the type of M. *)
    val checkI : int -> rho -> gamma -> exp -> value option

    (* ρ; Γ |- D  ⇒ Γ'. Checks that D is a valid declaration and
     * extends Γ to Γ'. *)
    val checkD : int -> rho -> gamma -> decl -> gamma option
  end
end
