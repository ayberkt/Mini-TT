structure S = Syntax

signature MONAD =
sig
  type 'a t

  val >>= : ('a t * ('a -> 'b t)) -> 'b t

  val return : 'a -> 'a t
  val fail   : S.name -> 'a t
end

structure Result : MONAD =
struct
  datatype 'a result = SUCCESS of 'a | FAIL of S.name
  type 'a t = 'a result

  fun >>= (SUCCESS x, k) = k x
    | >>= (FAIL s, _) = FAIL s

  val return = fn x => SUCCESS x
  val fail = fn x => FAIL x
end
