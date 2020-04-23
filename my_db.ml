type id = int ;;
let dbsize = 100 ;;

let db : (int, (string * int)) Hashtbl.t =
  Hashtbl.create dbsize ;;

let create (id : id) (name : string) : unit =
  Hashtbl.add db id (name, 0) ;;

let exists (id : id) : bool =
  Hashtbl.mem db id ;;

let balance (id : id) : int =
  snd (Hashtbl.find db id) ;;

let name (id : id) : string =
  fst (Hashtbl.find db id) ;;

let update (id : id) (bal : int) : unit =
  let nam = name id in
  Hashtbl.replace db id (nam, bal) ;;

let close (id : id) : unit =
  Hashtbl.remove db id ;;

let dump =
  db
  |> Hashtbl.iter (fun i (nam, bal) ->
      Printf.printf "[%d] %s -> %d\n" i nam bal) ;;
