(*
 * Copyright (C) 2007-9 Skydeck, Inc
 * Copyright (C) 2010 Jake Donham
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 *)

type unop =
    | Jdelete
    | Jvoid
    | Jtypeof
    | Jadd2_pre
    | Jsub2_pre
    | Jadd_pre
    | Jsub_pre
    | Jtilde
    | Jnot
    | Jadd2_post
    | Jsub2_post

and binop =
    | Jhashref
    | Jmul
    | Jdiv
    | Jmod
    | Jadd
    | Jsub
    | Jlt
    | Jgt
    | Jleq
    | Jgeq
    | Jlsr
    | Jlsl
    | Jasr
    | Jeq
    | Jneq
    | Jinstanceof
    | Jseq
    | Jsneq
    | Jland
    | Jlor
    | Jand
    | Jxor
    | Jor
    | Jcomma
    | Jassign
    | Jmul_assign
    | Jdiv_assign
    | Jmod_assign
    | Jadd_assign
    | Jsub_assign
    | Jlsl_assign
    | Jlsr_assign
    | Jasr_assign
    | Jand_assign
    | Jxor_assign
    | Jor_assign

type exp =
  | Jthis
  | Jvar of string
  | Jarray of exp
  | Jobject of (exp * exp) list
  | Jstring of string * bool (* true if double-quoted *)
  | Jnum of string
  | Jnull
  | Jbool of bool
  | Jregexp of string * string
  | Jfun of string option * string list * stmt
  | Jfieldref of exp * string
  | Junop of unop * exp
  | Jbinop of binop * exp * exp
  | Jite of exp * exp * exp
  | Jcall of exp * exp
  | Jnew of exp * exp option
  | Jexp_nil
  | Jexp_cons of exp * exp

and stmt =
  | Jvars of (string * exp option) list
  | Jfuns of string * string list * stmt
  | Jreturn of exp option
  | Jcontinue of string option
  | Jbreak of string option
  | Jswitch of exp * (exp * stmt) list * stmt
  | Jites of exp * stmt * stmt option
  | Jthrow of exp
  | Jexps of exp
  | Jtrycatch of stmt * (string * stmt) option * stmt
  | Jfor of (string * exp option) list * exp option * exp option * exp option * stmt
  | Jdowhile of stmt * exp
  | Jwhile of exp * stmt
  | Jblock of stmt
  | Jwith of exp * stmt
  | Jlabel of string * stmt
  | Jstmt_nil
  | Jstmt_cons of stmt * stmt

type xxx

open Format

(* XXX figure out how Format works *)

(*
  precedence, see ECMA 262:
  http://www.ecma-international.org/publications/files/EMCA-ST/Ecma-262.pdf
*)
let p = 0
let pAssignment = 2
let pConditional = 4
let pLogicalOR = 6
let pLogicalAND = 8
let pBitwiseOR = 10
let pBitwiseXOR = 12
let pBitwiseAND = 14
let pEquality = 16
let pRelational = 18
let pShift = 20
let pAdditive = 22
let pMultiplicative = 24
let pUnary = 26
let pPostfix = 28
let pLeftHandSide = 30
let pCall = 32
let pMember = 34
let pPrimary = 36


module JSString =
struct
  external is_printable: char -> bool = "caml_is_printable"

  let escaped s =
    let buf = Buffer.create 0 in
    let escaped c =
      if c > 0xFF then Printf.bprintf buf "\\u%04X" c
      else
        match Char.chr c with
          | '\'' -> Buffer.add_string buf "\\'"
          | '"' -> Buffer.add_string buf "\\\""
          | '\\' -> Buffer.add_string buf "\\\\"
          | '\n' -> Buffer.add_string buf "\\n"
          | '\t' -> Buffer.add_string buf "\\t"
          | '\r' -> Buffer.add_string buf "\\r"
          | '\b' -> Buffer.add_string buf "\\b"
          | c ->
              if is_printable c
              then Buffer.add_char buf c
              else Printf.bprintf buf "\\x%02X" (Char.code c) in
    Array.iter escaped (Utf8.to_int_array s 0 (String.length s));
    Buffer.contents buf
end

let id ppf i = fprintf ppf "%s" i

let ids ppf is =
  let com = ref false in
  List.iter
    (fun i ->
      if !com then fprintf ppf ",@ " else com := true;
      fprintf ppf "%a" id i)
    is

let is_postop = function
  | Jadd2_post | Jsub2_post -> true
  | _ -> false

let unop_op = function
  | Jdelete -> "delete"
  | Jvoid -> "void"
  | Jtypeof -> "typeof"
  | Jadd2_pre -> "++"
  | Jsub2_pre -> "--"
  | Jadd_pre -> "+"
  | Jsub_pre -> "-"
  | Jtilde -> "~"
  | Jnot -> "!"
  | Jadd2_post -> "++"
  | Jsub2_post -> "--"

let binop_op = function
  | Jmul -> "*"
  | Jdiv -> "/"
  | Jmod -> "%"
  | Jadd -> "+"
  | Jsub -> "-"
  | Jlsr -> ">>"
  | Jlsl -> "<<"
  | Jasr -> ">>>"
  | Jlt -> "<"
  | Jgt -> ">"
  | Jleq -> "<="
  | Jgeq -> ">="
  | Jinstanceof -> assert false
  | Jeq -> "=="
  | Jneq -> "!="
  | Jseq -> "==="
  | Jsneq -> "!=="
  | Jand -> "&"
  | Jxor -> "^"
  | Jor -> "|"
  | Jland -> "&&"
  | Jlor -> "||"
  | Jcomma -> ","
  | Jhashref -> assert false
  | Jassign -> "="
  | Jmul_assign -> "*="
  | Jdiv_assign -> "/="
  | Jmod_assign -> "%="
  | Jadd_assign -> "+="
  | Jsub_assign -> "-="
  | Jlsl_assign -> "<<="
  | Jlsr_assign -> ">>="
  | Jasr_assign -> ">>>="
  | Jand_assign -> "&="
  | Jxor_assign -> "^="
  | Jor_assign -> "|="

let binop_prec = function
  | Jeq -> pEquality
  | Jneq -> pEquality
  | Jseq -> pEquality
  | Jsneq -> pEquality
  | Jhashref -> pCall
  | Jlt -> pRelational
  | Jgt -> pRelational
  | Jleq -> pRelational
  | Jgeq -> pRelational
  | Jinstanceof -> pRelational
  | Jlsr -> pShift
  | Jlsl -> pShift
  | Jasr -> pShift
  | Jmul -> pMultiplicative
  | Jdiv -> pMultiplicative
  | Jmod -> pMultiplicative
  | Jadd -> pAdditive
  | Jsub -> pAdditive
  | Jand -> pBitwiseAND
  | Jxor -> pBitwiseXOR
  | Jor -> pBitwiseOR
  | Jland -> pLogicalAND
  | Jlor -> pLogicalOR
  | Jcomma -> p
  | Jassign -> pAssignment
  | Jmul_assign -> pAssignment
  | Jdiv_assign -> pAssignment
  | Jmod_assign -> pAssignment
  | Jadd_assign -> pAssignment
  | Jsub_assign -> pAssignment
  | Jlsl_assign -> pAssignment
  | Jlsr_assign -> pAssignment
  | Jasr_assign -> pAssignment
  | Jand_assign -> pAssignment
  | Jxor_assign -> pAssignment
  | Jor_assign -> pAssignment

let prec = function
  | Jthis -> pPrimary
  | Jvar _ -> pPrimary
  | Jarray _ -> pPrimary
  | Jobject _ -> pPrimary
  | Jstring _ -> pPrimary
  | Jnum _ -> pPrimary
  | Jnull _ -> pPrimary
  | Jfun _ -> pPrimary
  | Jbool _ -> pPrimary
  | Jregexp _ -> pPrimary

  | Jfieldref _ -> pMember
  | Jnew _ -> pMember

  | Junop (op, _) -> if is_postop op then pPostfix else pUnary
  | Jbinop (op, _, _) -> binop_prec op

  | Jite _ -> pConditional
  | Jcall _ -> pCall

  | Jexp_nil -> assert false
  | Jexp_cons _ -> assert false

let opt f ppf x =
  match x with
    | None -> ()
    | Some x -> f ppf x

let opt_nbsp f ppf x =
  match x with
    | None -> ()
    | Some x ->
        fprintf ppf " ";
        f ppf x

let rec stmt_iter f = function
  | Jstmt_nil -> ()
  | Jstmt_cons (s1, s2) ->
      stmt_iter f s1;
      stmt_iter f s2
  | s -> f s

let rec expp pr ppf e =
  if prec e < pr
  then fprintf ppf "(@[%a@])" exp e
  else exp ppf e

and exp ppf = function
  | Jthis  -> fprintf ppf "this"
  | Jvar i -> fprintf ppf "%s" i
  | Jarray es -> fprintf ppf "@[<hv>[@;<1 2>%a@ ]@]" aexps es
  | Jobject kvs ->
      let keyvals ppf kvs =
        let com = ref false in
        List.iter
          (fun (k, v) ->
            if !com then fprintf ppf ",@;<1 2>" else com := true;
            fprintf ppf "@[<hv 2>%a:@ %a@]" (expp pAssignment) k (expp pAssignment) v)
          kvs in
      fprintf ppf "@[<hv>{@;<1 2>%a@ }@]" keyvals kvs
  | Jstring (s, false) -> fprintf ppf "\"%s\"" (JSString.escaped s)
  | Jstring (s, true) -> fprintf ppf "\'%s\'" (JSString.escaped s)
  | Jnum n -> fprintf ppf "%s" n
  | Jnull -> fprintf ppf "null"
  | Jbool b -> fprintf ppf "%B" b
  | Jregexp (r, f) -> fprintf ppf "/%s/%s" r f
  | Jfun (io, is, ss) ->
      fprintf ppf "@[<hv>function %a@[<hv 1>(%a)@]%a@]" (opt_nbsp id) io ids is block ss

  | Jfieldref (e, i) -> fprintf ppf "@[<hv 2>%a.@,%s@]" (expp pMember) e i

  | Junop (op, e) ->
      if is_postop op
      then
        begin
          fprintf ppf "@[%a%s@]" (expp pPostfix) e (unop_op op)
        end
      else
        begin
          match op with
            | Jdelete | Jvoid | Jtypeof -> fprintf ppf "@[%s %a@]" (unop_op op) (expp pUnary) e
            | _ -> fprintf ppf "@[%s%a@]" (unop_op op) (expp pUnary) e
        end

  | Jbinop (op, e1, e2) ->
      begin
        match op with
          | Jhashref -> fprintf ppf "@[%a[%a]@]" (expp pCall) e1 (expp p) e2
          | Jcomma -> fprintf ppf "@[%a, %a@]" (expp p) e1 (expp pAssignment) e2
          | _ ->
              let prec = binop_prec op in
              fprintf ppf "@[<hv 2>%a %s@ %a@]" (expp prec) e1 (binop_op op) (expp (prec + 2)) e2
      end

  | Jite (i, t, e) ->
      fprintf ppf "@[<hv 2>%a ?@ %a :@ %a@]"
        (expp pLogicalOR) i
        (expp pAssignment) t
        (expp pAssignment) e

  | Jcall (e, es) -> fprintf ppf "@[%a@[<hov 1>(%a)@]@]" (expp pCall) e exps es

  | Jnew (e, None) -> fprintf ppf "@[new %a@]" (expp pMember) e
  | Jnew (e, Some es) -> fprintf ppf "@[new %a@[<hov 1>(%a)@]@]" (expp pMember) e exps es

  | Jexp_nil -> assert false
  | Jexp_cons _ -> assert false

and exps ppf e =
  match e with
    | Jexp_nil -> ()
    | Jexp_cons (e1, e2) ->
        exps ppf e1;
        fprintf ppf ",@ ";
        exps ppf e2;
    | _ ->
        (expp pAssignment) ppf e

and aexps ppf e =
  match e with
    | Jexp_nil -> ()
    | Jexp_cons (e1, e2) ->
        aexps ppf e1;
        fprintf ppf ",@;<1 2>";
        aexps ppf e2;
    | _ ->
        (expp pAssignment) ppf e

and variableDeclarationList ppf = function
  | [ (i, None) ] -> fprintf ppf "@[<hv 2>var %s@]" i
  | [ (i, Some e) ] -> fprintf ppf "@[<hv 2>var %s =@ %a@]" i (expp pAssignment) e
  | vars ->
      let fvars ppf vars =
        let comma = ref false in
        List.iter
          (fun (i, e) ->
             if !comma then fprintf ppf ",@ " else comma := true;
             match e with
               | Some e -> fprintf ppf "%s =@;<1 2>%a" i (expp pAssignment) e
               | None -> fprintf ppf "%s" i)
          vars in
      fprintf ppf "@[<hv 2>var@ %a@]" fvars vars

and stmt ppf = function
  | Jvars vars ->
      fprintf ppf "%a;" variableDeclarationList vars

  | Jfuns (i, is, ss) ->
      fprintf ppf "@[<hv>function %s @[<hv 1>(%a)@]%a@]" i ids is block ss

  | Jreturn e -> fprintf ppf "@[<h>return%a;@]" (opt_nbsp (expp p)) e
  | Jcontinue i -> fprintf ppf "@[<h>continue%a;@]" (opt_nbsp id) i
  | Jbreak i -> fprintf ppf "@[<h>break%a;@]" (opt_nbsp id) i

  | Jites (i, t, None) ->
      fprintf ppf
        "@[<hv>if (%a)%a@]"
        (expp p) i maybe_block t

  | Jites (i, t, Some e) ->
      fprintf ppf
        "@[<hv>if (%a)%a@ else%a@]"
        (expp p) i maybe_block t maybe_block e

  | Jswitch (e, cs, fss) ->
      let cases ppf (cs, fss) =
        let spc = ref false in
        List.iter
          (fun (i, ss) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv>case %a:%a@]"
              (expp p) i ind_stmts ss)
          cs;
        match fss with
          | Jstmt_nil _ -> ()
          | _ ->
              if !spc then fprintf ppf "@ " else spc := true;
              fprintf ppf "@[<hv>default:%a@]" ind_stmts fss in
      fprintf ppf
        "@[<hv>switch (%a)@ {@ %a@ }@]"
        (expp p) e cases (cs, fss)

  | Jthrow e -> fprintf ppf "@[throw %a;@]" (expp p) e

  | Jexps (Jcall (Jfun _, _) as e) -> fprintf ppf "@[(%a);@]" (expp p) e
  | Jexps e -> fprintf ppf "@[%a;@]" (expp p) e

  | Jtrycatch (ss, Some (ci, css), Jstmt_nil _) ->
      fprintf ppf "@[<hv>try%a@ catch (%s)%a@]" block ss ci block css
  | Jtrycatch (ss, None, fss) ->
      fprintf ppf "@[<hv>try%a@ finally%a@]" block ss block fss
  | Jtrycatch (ss, Some (ci, css), fss) ->
      fprintf ppf "@[<hv>try%a@ catch (%s)%a finally%a@]" block ss ci block css block fss

  | Jfor ([], e1, e2, e3, s) ->
      fprintf ppf "@[<hv>for @[<hv 1>(%a;@ %a;@ %a)@]%a@]" (opt (expp p)) e1 (opt (expp p)) e2 (opt (expp p)) e3 maybe_block s
  | Jfor (vars, None, e2, e3, s) ->
      fprintf ppf "@[<hv>for @[<hv 1>(%a;@ %a;@ %a)@]%a@]" variableDeclarationList vars (opt (expp p)) e2 (opt (expp p)) e3 maybe_block s
  | Jfor _ -> assert false

  | Jdowhile (s, e) ->
      fprintf ppf "@[<hv>do%a@ while (%a);@]" maybe_block s (expp p) e

  | Jwhile (e, s) ->
      fprintf ppf "@[<hv>while (%a)%a@]" (expp p) e maybe_block s

  | Jblock ss -> fprintf ppf "@[<hv>{%a@ }@]" ind_stmts ss
  | Jwith (e, s) -> fprintf ppf "@[<hv>with (%a)%a@]" (expp p) e maybe_block s
  | Jlabel (i, s) -> fprintf ppf "@[<hv>%s:%a@]" i maybe_block s

  | (Jstmt_nil _ | Jstmt_cons _) as ss ->
      stmts ppf ss

and block ppf ss = fprintf ppf " {%a@ }" ind_stmts ss

and maybe_block ppf = function
  | Jblock ss -> block ppf ss
  | Jstmt_nil -> fprintf ppf ";"
  | Jstmt_cons _ as s -> block ppf (Jblock s)
  | s -> fprintf ppf "@;<1 2>%a" stmt s

and ind_stmts ppf ss =
  stmt_iter (fun s -> fprintf ppf "@;<1 2>%a" stmt s) ss

and stmts ppf ss =
  let spc = ref false in
  stmt_iter
    (fun s ->
      if !spc then fprintf ppf "@ " else spc := true;
      stmt ppf s)
    ss

let escaped = JSString.escaped

let to_string t =
  stmt str_formatter t;
  flush_str_formatter ()
