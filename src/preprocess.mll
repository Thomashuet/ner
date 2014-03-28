{

open Batteries

let decode =
  Netencoding.Html.decode
    ~in_enc:Netconversion.(`Enc_utf8)
    ~out_enc:Netconversion.(`Enc_utf8)
    ()

open CamomileLibraryDefault.Camomile
module CM = CaseMap.Make(UTF8)

(*
let capitalize (s : string) : string =
  if s = "" then "" else
  let first = UTF8.get s 0 in
  let capital = CM.uppercase (UTF8.init 1 (fun _ -> first)) in
  UTF8.init (UTF8.length s) (function 0 -> UTF8.get capital 0 | i -> UTF8.get s i)
*)
let capitalize = CM.capitalize

let underscores =
  Str.global_replace (Str.regexp "^_\\|_$") ""
% Str.global_replace (Str.regexp "[_ ]+") "_"

(* https://en.wikipedia.org/wiki/Help:Link#Conversion_to_canonical_form *)
let canonicalize = capitalize % underscores % decode

(* Change "François {{1er}}" to "François 1er" *)
let clean = Str.global_replace (Str.regexp "{{\\(.*|\\)*\\([^|]*\\)}}") "\\2"

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

rule main acc = parse
| "<page>" { main (page None acc lexbuf) lexbuf }
| _  { main acc lexbuf }
| eof { acc }

and page p acc = parse
| "</page>" { acc }
| "<title>" { page (Some (pagename lexbuf)) acc lexbuf }
| "<redirect title=\"" {
  let target = pagename lexbuf in
  skip lexbuf;
  match p with
  | None -> acc
  | Some source ->
    let redirect, name, link = acc in
    M.add (canonicalize source) (canonicalize target) redirect, name, link
}
| "<text xml:space=\"preserve\">" {
  match p with
  | None -> acc
  | Some title ->
    wikitext title acc lexbuf
}
| "<ns>" {
  if num lexbuf = 0 then page p acc lexbuf
  else (skip lexbuf; acc) 
}
| _ { page p acc lexbuf }

and pagename = parse
| [^'<' '"' '|' ']']* as t { t }

and num = parse
| ['0'-'9']+ as n { int_of_string n }

and skip = parse
| "</page>" { () }
| _ { skip lexbuf }

and wikitext p acc = parse
| "</text>" { acc }
| "&lt;nowiki&gt;" { nowiki lexbuf; wikitext p acc lexbuf }
| "[[" {
  let title = pagename lexbuf in
  if String.contains title ':'
  || String.contains title '#'
  || String.contains title '\n'
  then wikitext p acc lexbuf
  else wikitext p (anchor p title acc lexbuf) lexbuf
}
| _ { wikitext p acc lexbuf }

and anchor source target acc = parse
| "|"? (([^']' '|' '\n']|']'[^']' '|' '\n'])* as a) "]]" (['a'-'z' 'A'-'Z' '\128'-'\255']* as b) {
  let text = clean ((if a = "" then target else a) ^ b) in
  let redirect, name, link = acc in
  redirect
, (increment text (canonicalize target) name)
, (extend (canonicalize source) (canonicalize target) link)
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
  let ic, close =
    if input = "-" then stdin, ignore else
    let ic = open_in input in ic, fun () -> close_in ic
  in
  let lexbuf = Lexing.from_input ic in
  let redirect, name, link = main (M.empty, M.empty, M.empty) lexbuf in
  let () = close () in
  let oc, close =
    if name_out = "-" then stdout, ignore else
    let oc = open_out name_out in oc, fun () -> close_out oc
  in
  let proba m =
    let tot = float (M.fold (fun _ -> (+)) m 0) in
    M.add "<Other>" (float (M.cardinal m) /. tot)
      (M.filterv ((<) 0.)
        (M.map (fun a -> float (a - 1) /. tot) m))
  in
  M.iter
    (fun text m ->
      M.iter
        (fun entity count ->
          output_string oc (text^"\t"^follow redirect entity^"\t"^string_of_float count^"\n"))
        m)
    (M.filterv (fun v -> M.cardinal v > 1) (M.map proba name));
  close ();
  let oc, close =
    if link_out = "-" then stdout, ignore else
    let oc = open_out link_out in oc, fun () -> close_out oc
  in
  M.iter
    (fun source s ->
      S.iter (fun target -> output_string oc (source^"\t"^follow redirect target^"\n")) s)
    link;
  close ()

let () =
  if Array.length Sys.argv = 4 then run Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
  else print_endline "usage: preprocess pages-articles.xml names.out links.out"

}
