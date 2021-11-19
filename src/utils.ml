module String = struct
  include String

  let sub_end s start = String.sub s start (String.length s - start)
end

module Unix = struct
  include Unix

  module Env = struct
    type t = (string * string) array

    let get () =
      let split str =
        let rec aux i buf =
          if Char.equal '=' str.[i] then
            let name = Buffer.contents buf
            and value =
              try String.sub_end str (i + 1)
              with Invalid_argument _ -> String.sub_end str i
            in
            (name, value)
          else (
            Buffer.add_char buf str.[i];
            aux (i + 1) buf)
        in
        aux 0 (Buffer.create 7)
      in

      Unix.environment () |> Array.map split

    let set = Array.iter (fun (name, value) -> Unix.putenv name value)

    let merge = Array.append

    let with_env env ~cmd =
      let initial_env = get () in
      let temp_env = merge initial_env env in
      set temp_env;
      let exit_code = Sys.command cmd in
      set initial_env;
      exit_code
  end
end
