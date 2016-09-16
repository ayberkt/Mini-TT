structure S = Syntax

signature MONAD =
sig
  type 'a result

  val >>= : ('a result * ('a -> 'b result)) -> 'b result

  val return : 'a -> 'a result
  val fail   : S.name -> 'a result
end
