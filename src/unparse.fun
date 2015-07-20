structure Unparse :> UNPARSE =
struct
  datatype associativity = Left | Right | Non

  datatype 'a item =
      Unit of 'a
    | Paren of 'a seq
  withtype 'a seq = 'a item list

  datatype part_type =
      ATM
    | ADJ
    | PREFIX of int
    | INFIX of associativity * int
    | POSTFIX of int

  fun cons x xs = x :: xs

  exception NoPrecedence

  fun prec (PREFIX p) = p
    | prec (INFIX (_, p)) = p
    | prec (POSTFIX p) = p
    | prec _ = raise NoPrecedence

  fun assoc (INFIX (a, _)) = a
    | assoc _ = Non

  fun right (t, t', f') =
    let
      fun accept (_, ATM) = true
        | accept (ADJ, _) = false
        | accept (_, ADJ) = true
        | accept (_, PREFIX _) = true
        | accept (PREFIX p, t) = prec t > p
        | accept (INFIX (a,p), t) =
          if prec t > p then
            true
          else if p > prec t then
            false
          else
            a = Right andalso assoc t = Right
        | accept _ = false
    in
      if accept (t, t') then
        f'
      else
        cons (Paren (f' []))
    end

  fun left (t, t', f') =
    let
      fun accept (_,ATM) = true
        | accept (_,ADJ) = true
        | accept (ADJ,_) = false
        | accept (_,POSTFIX _) = true
        | accept (POSTFIX p,t) = prec(t) > p
        | accept (INFIX (a,p),t) =
          if prec t > p then
            true
          else if p > prec t then
            false
          else
            a = Right andalso assoc t = Left
        | accept _ = false
    in
      if accept (t, t') then
        f'
      else
        cons (Paren (f' []))
    end

  type 'a part =
    {partType : part_type,
     prepend : 'a seq -> 'a seq}

  fun atom c =
    {partType = ATM,
     prepend = cons (Unit c)}

  fun prefix (p, c) {partType, prepend} =
    {partType = PREFIX p,
     prepend = cons (Unit c) o right (PREFIX p, partType, prepend)}

  fun infix' (a, p, c) (pt1 : 'a part, pt2 : 'a part) =
    let
      val t = INFIX (a, p)
    in
      {partType = t,
       prepend =
         left (t, #partType pt1, #prepend pt1)
         o cons (Unit c)
         o right (t, #partType pt2, #prepend pt2)}
    end

  fun postfix (p, c) {partType, prepend} =
    {partType = POSTFIX p,
     prepend = left (POSTFIX p, partType, prepend) o cons (Unit c)}

  fun adj (pt1 : 'a part, pt2 : 'a part) =
    {partType = ADJ,
     prepend =
       left (ADJ, #partType pt1, #prepend pt1)
         o right (ADJ, #partType pt2, #prepend pt2)}

  fun done {prepend,partType} = prepend []

  fun foldItem (cseq : 'a list -> 'a) (cparen : 'a -> 'a) (s : 'a seq) : 'a =
    let
      fun citem (Unit a) = a
        | citem (Paren s) = cparen (cseq (map citem s))
    in
      cseq (map citem s)
    end

  fun spaces [] = ""
    | spaces (h::t) = h ^ foldr op^ "" (map (fn s => " " ^ s) t)

  val parens = foldItem spaces (fn s => "(" ^ s ^ ")")
end
