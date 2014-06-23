{

open Batteries

let decode =
  Netencoding.Html.decode
    ~in_enc:Netconversion.(`Enc_utf8)
    ~out_enc:Netconversion.(`Enc_utf8)
    ()

let strip =
  let tag = Str.regexp "<[^>]*>" in
  Str.global_replace tag "";;

(* Change "  François {{1er}}" to "François 1er" *)
let clean =
  let nbsp = Str.regexp "\194\160" in
  let ends = Str.regexp "^[ \t\n\r]*\\|[ \t\n\r]*$" in
  let block = Str.regexp "{{\\(.*|\\)*\\([^|]*\\)}}" in
  Str.global_replace ends ""
% Str.global_replace nbsp " "
% Str.global_replace block "\\2"
% decode
% strip
% decode

open CamomileLibraryDefault.Camomile
module CM = CaseMap.Make(UTF8)

let capitalize (s : string) : string =
  if s = "" then "" else
  let first = UTF8.get s 0 in
  let capital = CM.uppercase (UTF8.init 1 (fun _ -> first)) in
  UTF8.init (UTF8.length s)
    (function 0 -> UTF8.get capital 0 | i -> UTF8.get s i)
(* Not available yet
let capitalize = CM.capitalize
*)

let underscores =
  let ends = Str.regexp "^_\\|_$" in
  let spaces = Str.regexp "[_ ]+" in
  Str.global_replace ends ""
% Str.global_replace spaces "_"

(* https://en.wikipedia.org/wiki/Help:Link#Conversion_to_canonical_form *)
let canonicalize = capitalize % underscores % clean

open Lexing

module M = Map.Make(String)
module S = Set.Make(String)

(*
* Types of the computed dictionaries:
* redirect : string M.t
* name : int M.t M.t
* link : string S.t M.t
*)

let extend k v m =
  M.modify_def S.empty k (S.add v) m

let increment k v m =
  M.modify_def M.empty k (M.modify_def 0 v ((+) 1)) m

}

(*
acc : ('a, 'b, 'c)
f : (string -> string -> 'a -> 'a,
     string -> string -> 'b -> 'b,
     string -> string -> 'c -> 'c)
'a = accumulator type for redirections
'b = accumulator type for dictionary (name -> entity)
'c = accumulator type for links
*)

rule main acc f = parse
| "<page>" { main (page None acc f lexbuf) f lexbuf }
| _  { main acc f lexbuf }
| eof { acc }

and page p acc f = parse
| "</page>" { acc }
| "<title>" { page (Some (pagename lexbuf)) acc f lexbuf }
| "<redirect title=\"" {
  let target = pagename lexbuf in
  skip lexbuf;
  match p with
  | None -> acc
  | Some source ->
    let redirect, name, link = acc in
    let f, _, _ = f in
    f source target redirect, name, link
}
| "<text xml:space=\"preserve\">" {
  match p with
  | None -> acc
  | Some title ->
    wikitext title acc f lexbuf
}
| "<ns>" {
  if num lexbuf = 0 then page p acc f lexbuf
  else (skip lexbuf; acc) 
}
| _ { page p acc f lexbuf }

and pagename = parse
| [^'<' '"' '|' ']']* as t { t }

and num = parse
| ['0'-'9']+ as n { int_of_string n }

and skip = parse
| "</page>" { () }
| _ { skip lexbuf }

and wikitext p acc f = parse
| "</text>" { acc }
| "&lt;nowiki&gt;" { nowiki lexbuf; wikitext p acc f lexbuf }
| "[[" {
  let title = pagename lexbuf in
  if String.contains title ':'
  || String.contains title '#'
  || String.contains title '\n'
  then wikitext p acc f lexbuf
  else wikitext p (anchor p title acc f lexbuf) f lexbuf
}
| _ { wikitext p acc f lexbuf }

and anchor source target acc f = parse
| "|"? (([^']' '|' '\n']|']'[^']' '|' '\n'])* as a) "]]"
  (['a'-'z' 'A'-'Z' '\128'-'\255']* as b)
{
  let text = (if a = "" then target else a) ^ b in
  let redirect, name, link = acc in
  let _, f, g = f in
  redirect
, f text target name
, g source target link
}
| _ { acc }

and nowiki = parse
| "&lt;/nowiki&gt;" { () }
| _ { nowiki lexbuf }

{

let rec follow redirection page =
  if M.mem page redirection then follow redirection (M.find page redirection)
  else page

let run input name_out link_out =
  let ic = open_in input in
  let lexbuf = Lexing.from_input ic in
  (* build redirections and dictionary (name -> entity) *)
  let redirect, name, () =
    main (M.empty, M.empty, ())
      ((fun source target m ->
        M.add (canonicalize source) (canonicalize target) m),
      (fun text target m -> increment (clean text) (canonicalize target) m),
      fun _ _ _ -> ())
      lexbuf
  in
  close_in ic;
  (* merge redirected *)
  let merge k v =
    let k = follow redirect k in
    M.modify_def v k ((+) v)
  in
  let name = M.map (fun m -> M.fold merge m M.empty) name in
  let oc, close =
    if name_out = "-" then stdout, ignore else
    let oc = open_out name_out in oc, fun () -> close_out oc
  in
  let proba text m =
    let tot = float (M.fold (fun _ -> (+)) m 0) in
    if tot > 12. then
      M.add ("<Other>:"^CM.casefolding text) (float (M.cardinal m) /. tot)
	(M.filterv ((<) 0.)
	  (M.map (fun a -> float (a - 1) /. tot) m))
    else M.empty
  in
  (* print dictionary (name -> entity) *)
  M.iter
    (fun text m ->
      M.iter
        (fun entity count ->
          output_string oc
            (text^"\t"^entity^"\t"^string_of_float count^"\n"))
        m)
    (M.filterv (fun v -> M.cardinal v > 1) (M.mapi proba name));
  close ();
  let ic = open_in input in
  let lexbuf = Lexing.from_input ic in
  let oc, close =
    if link_out = "-" then stdout, ignore else
    let oc = open_out link_out in oc, fun () -> close_out oc
  in
  (* print links *)
  let (), (), () =
    main ((), (), ())
      ((fun _ _ _ -> ()),
      (fun _ _ _ -> ()),
      fun source target () ->
        output_string oc
          (canonicalize source^"\t"^follow redirect (canonicalize target)^"\n"))
      lexbuf
  in
  close ();
  close_in ic

let () =
  if Array.length Sys.argv = 4 then run Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
  else print_endline "usage: preprocess pages-articles.xml names.out links.out"

}
