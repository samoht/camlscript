(*
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Camlp4.PreCast
open Printf 

module Q  = Syntax.Quotation
module AQ = Syntax.AntiquotSyntax

let destruct_aq s =
  try
    let pos = String.index s ':' in
    let space = String.index s ' ' in
    if space < pos then raise Not_found;
    let len = String.length s in
    let name = String.sub s 0 pos
    and code = String.sub s (pos + 1) (len - pos - 1) in
    name, code
  with Not_found ->
    "", s

let aq_expander =
object
  inherit Ast.map as super
  method expr =
    function
      | Ast.ExAnt (_loc, s) ->
          let n, c = destruct_aq s in
          let e = AQ.parse_expr _loc c in
          begin match n with
            | "node" -> (* XXX *) assert false
            | "" -> e
            | t ->
              eprintf "[ERROR] \"%s\" is not a valid tag. Valid tags are [node]\n" t;
              Loc.raise _loc Parsing.Parse_error
          end
      | e -> super#expr e
end

(* XXX: horrible hack to be able to read cmjs files from ocamljs 0.3 *)
let stmt_of_cmjs file =
  let cmjs_magic_number = "Caml1999J006" in
  let chan = open_in file in  
  let buffer = String.create (String.length cmjs_magic_number) in
  really_input chan buffer 0 (String.length cmjs_magic_number);
  if buffer = cmjs_magic_number then begin
    let _ = input_binary_int chan in
    let ast = (input_value chan : Jslib_ast.stmt) in
    let ast = match ast with
      | Jslib_ast.Jvars (loc, [_, Some exp]) -> Jslib_ast.Jexps (loc, exp)
      | _ -> assert false in
    close_in chan;
    ast
  end else
    failwith (sprintf "%s is not a valid cmjs file. Was it compiled with ocamljs 0.3 ?" file)

let parse_quot_string str =
  let src_file = Filename.temp_file "ocamljs" ".ml" in
  let dst_file = sprintf "%s.cmjs" (Filename.chop_suffix src_file ".ml") in
  let chan = open_out src_file in
  output_string chan str;
  close_out chan;
  let cmd = sprintf "ocamljs -c %s" src_file in
  match Sys.command cmd with
    | 0 -> stmt_of_cmjs dst_file
    | n -> eprintf "%s (exit %d)\n" cmd n; failwith cmd

let expand_expr _loc _ s =
  let ast = parse_quot_string s in
  let meta_ast = Camlscript_ast.meta_stmt _loc ast in
  aq_expander#expr meta_ast

let expand_str_item _loc _ s =
  let exp_ast = expand_expr _loc None s in
  <:str_item< $exp:exp_ast$ >>

let () =
  Q.add "mls" Q.DynAst.expr_tag expand_expr;
  Q.add "mls" Q.DynAst.str_item_tag expand_str_item
