signature UNPARSE =
sig
  datatype associativity = Left | Right | Non

  datatype 'a item =
      Unit of 'a
    | Paren of 'a item list

  type 'a seq = 'a item list

  type 'a part
  val atom : 'a -> 'a part
  val prefix : int * 'a -> 'a part -> 'a part
  val infix' : associativity * int * 'a -> 'a part * 'a part -> 'a part
  val postfix : int * 'a -> 'a part -> 'a part
  val adj : 'a part * 'a part -> 'a part
  val done : 'a part -> 'a seq
  val parens : string seq -> string
end
