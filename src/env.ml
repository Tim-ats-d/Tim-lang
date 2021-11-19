open Base

type t = (string, Value.t) Hashtbl.t

let set t ~name ~value = Hashtbl.set t ~key:name ~data:value

let find_exn t ~name = Hashtbl.find_exn t name

let empty : t = Hashtbl.create (module String)
