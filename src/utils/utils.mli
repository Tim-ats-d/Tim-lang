module String : sig
  include module type of String

  val sub_end : string -> int -> string
end

module Unix : sig
  include module type of Unix

  module Env : sig
    type t = (string * string) array

    val get : unit -> t

    val set : t -> unit

    val merge : t -> t -> t

    val with_env : t -> f:(unit -> unit) -> unit
  end
end
