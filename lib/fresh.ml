type id = int

let current () : id =
  !(Symbol.Fresh.counter)

let seed (id : id) : unit =
  Symbol.Fresh.ensure_ge (id + 1)

let seed_many (ids : id list) : unit =
  List.iter seed ids

let fresh ?(hint = "x") () : id =
  Symbol.Fresh.gensym ~hint ()
