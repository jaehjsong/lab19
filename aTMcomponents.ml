open Printf ;;
open Scanf ;;

module D = My_db ;;

(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;

type account_spec = {name : string; id : id; balance : int} ;;

let initialize (initial : account_spec list) : unit =
  initial
  |> List.iter (fun {name; id; balance}
                -> D.create id name;
                  D.update id balance) ;;

let rec acquire_id () : id =
   printf "type your account number please: ";
   try
     id = read_int ();
     ignore (D.exists id); id
   with
   | Not_found
   | Failure _ -> printf "Account not found please try again";
                  acquire_id ;;

let rec acquire_amount () : int =
  printf "Enter amount of money for transaction";
  try
    amt = read_int ();
    if amt <= 0 then raise (Failure "amount is non-positive")
  with
  | _ -> printf "please amount a valid amount";
         acquire_amount () ;;

let acquire_act () : action =
  printf "Press the following for action: \"b\" for blanace;
\"w\" for withdraw; \"d\" for deposit; \"n\" for next; \"f\" for finished : %!"
    scanf " %c"
          (fun char -> match char with
                       | 'b' | 'B'        -> Balance
                       | '/' | 'x' | 'X'  -> Finished
                       | '='              -> Next
                       | 'w' | 'W' | '-'  -> Withdraw (acquire_amount ())
                       | 'd' | 'D' | '+'  -> Deposit (acquire_amount ())
                       | _                -> printf "  invalid choice\n";
                                             acquire_act () ) ;;

let get_balance (id : id) : int =
  D.balance id ;;

let get_name (id : id) : string =
  D.name id ;;

let update_balance (id : id) (bal : int) : unit =
  update id bal ;;

  let present_message (msg : string) : unit =
    printf "%s\n%!" msg ;;

  let deliver_cash (amount : int) : unit =
    printf "your cash: ";
    for _i = 1 to (amount / 20) do
      printf "[20 @ 20]"
    done;
    printf " and %d more\n" (amount mod 20) ;;
