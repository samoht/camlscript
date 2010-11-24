(*
 * This file is part of ocamljs, OCaml to Javascript compiler
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

(** XXX : we use a modified version of jslib_ast.incl here as we don't
    care about location informations. The only problem is that it will
    be difficult to mix jslib and mls quotations in the same program as
    they are compiled to a different AST ... so will surely need either
    fork jslib to adapt it to my needs or will try to find a good
    solution to not have to link with camlp4 (because of the Ast.loc
    stuff) *)

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

and exp =
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

val exp : Format.formatter -> exp -> unit
val stmt : Format.formatter -> stmt -> unit

val escaped : string -> string

val to_string : stmt -> string
