open Base

module type S1 = sig
  type t

  val next : t -> char
  val observe : t -> char 
end

module type S = sig
  type t

  val consume : t -> (string * char) option
end

module Make (S1 : S1) : S with type t := S1.t = struct
  let consume st = Some ("", '\x00') 
    (* let is_whitespace ch = ch == ' ' || ch == '\t' in *)
    (* let rec skip_whitespaces () = *)
    (*   if is_whitespace (S1.next st) then *)
    (*     () *)
    (*   else  *)
    (*     skip_whitespaces () *)
    (* in *)
    (* let rec skip_multiline_comment () = *)
    (*   if S1.next st == '*' then  *)
    (*     if S1.observe st == '/' then  *)
    (*       let _ = S1.next st in *)
    (*       () *)
    (*     else  *)
    (*       skip_multiline_comment () *)
    (* let is_multiline_comment = *)
    (*   if S1.observe st == '/' then  *)
    (*     let _  = S1.next st in *)
    (**)
    (**)
    (**)
    (**)
    (**)
    (* skip_whitespaces; *)
    (* Some ("", '\x00') *)
end

(* module StringState : sig *)
(*   include S *)
(**)
(*   val create : string -> t *)
(* end = struct *)
(*   type t = { contents : string; mutable ptr : int } *)
(**)
(*   let create contents = { contents; ptr = 0 } *)
(**)
(*   let next s =  *)
(*     if String.length contents == ptr then *)
(*       '\x00' *)
(*     else  *)
(**)
(*   let consume s =  *)
(**)
(**)
(* end *)
