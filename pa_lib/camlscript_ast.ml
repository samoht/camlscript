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

module M  = Jslib_ast.Meta.Make(Jslib_ast.Meta.MetaLoc) 

let reset_loc =
object
  inherit Ast.map as super
  method expr = function
      | <:expr@_loc< Jslib_ast.$n$ $t$ >> ->
        let t = Ast.list_of_expr t [] in
        let t = List.map super#expr t in
        (match t with
          | [] | [_] -> <:expr< Js.$n$ >>
          | _::t     -> <:expr< Js.$n$ $tup:Ast.exCom_of_list t$ >>)

      | <:expr@_loc< Jslib_ast.$n$ >> ->
        <:expr< Js.$n$ >>

      | e ->
        super#expr e
end

(* Lift the AST and forget about locations *)
let meta_stmt _loc ast =
  let meta_ast = M.Expr.meta_stmt _loc ast in
  reset_loc#expr meta_ast
  
