{

open Batteries

let decode =
  Netencoding.Html.decode
    ~in_enc:Netconversion.(`Enc_utf8)
    ~out_enc:Netconversion.(`Enc_utf8)
    ()
% Netencoding.Url.decode ~plus:false

let capitalize (s : string) : string =
  if s = "" then "" else
  let open CamomileLibraryDefault.Camomile in
  let module CM = CaseMap.Make(UTF8) in
  let first = UTF8.get s 0 in
  let capital = CM.uppercase (UTF8.init 1 (fun _ -> first)) in
  UTF8.init (UTF8.length s) (function 0 -> UTF8.get capital 0 | i -> UTF8.get s i)

let underscores =
  Str.global_replace (Str.regexp "^_\\|_$") ""
% Str.global_replace (Str.regexp "[_ ]+") "_"

let canonicalize = capitalize % underscores % decode

open Lexing

module M = Map.Make(String)

let find default k m =
  if M.mem k m then M.find k m
  else default

let increment k v m =
  let n = find M.empty k m in
  M.add k (M.add v (1 + find 0 v n) n) m

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
  let text = (if a = "" then target else a) ^ b in
  let redirect, name, link = acc in
  redirect
, (increment text (canonicalize target) name)
, (M.add (canonicalize source) (canonicalize target) link)
}
| _ { acc }

and nowiki = parse
| "&lt;/nowiki&gt;" { () }
| _ { nowiki lexbuf }

{

let run file =
  let lexbuf = Lexing.from_input (if file = "-" then stdin else open_in file) in
  ignore (main (M.empty, M.empty, M.empty) lexbuf)

let () = Arg.parse [] run "preprocess [file]"

}
