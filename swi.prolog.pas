unit swi.prolog;
interface
{$DEFINE WINDOWS}

//logtalk.org
// suchen nach todo:

(*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2023, University of Amsterdam
			      VU University Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE. *)


(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This number is incremented when the   SWI-Prolog PL_*() functions or one
of the data types is modified such that old binary extensions cannot run
reliably with the  current  version.  This   version  is  introduced  in
SWI-Prolog 8.1.30. The  most  recent   violation  of  compatibility  was
between versions 8.1.21 and 8.1.22  with   the  introduction of rational
numbers are atomic type.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

type  Float      = Double;


const PL_FLI_VERSION      = 2;		//* PL_*() functions
      PL_REC_VERSION      = 3;		//* PL_record_external(), fastrw
      PL_QLF_LOADVERSION  = 68;		//* load all versions later >= X
      PL_QLF_VERSION      = 68;   //* save version number

////////////////////////////////////////////////////////////////////////////////
// TERM TYPE CONSTANTS
////////////////////////////////////////////////////////////////////////////////
///
const
      PL_VARIABLE	 =  1;  //* nothing
      PL_ATOM		   =  2;	 //* const char *
      PL_INTEGER   =  3;	 //* int
      PL_RATIONAL	 =  4;	 //* rational number
      PL_FLOAT	   =  5;	 //* double
      PL_STRING	   =  6;  //* const char *
      PL_TERM		   =  7;

      PL_NIL		   =  8;		//* The constant []
      PL_BLOB		   =  9;		//* non-atom blob
      PL_LIST_PAIR = 10;		//* [_|_] term

 		(* PL_unify_term() *)
      PL_FUNCTOR	 = 11;		//* functor_t, arg ...
      PL_LIST		   = 12;		//* length, arg ...
      PL_CHARS	   = 13;		//* const char *
      PL_POINTER	 = 14;		// void * */

			(* PlArg::PlArg(text, type) *)
      PL_CODE_LIST = 15;		//* [ascii...]
      PL_CHAR_LIST = 16;		//* [h,e,l,l,o]
      PL_BOOL		   = 17;		//* PL_set_prolog_flag()
      PL_FUNCTOR_CHARS  = 18;	//* PL_unify_term()
      _PL_PREDICATE_INDICATOR = 19; //* predicate_t (Procedure)
      PL_SHORT	    = 20;	 //* short
      PL_INT		    = 21;	 //* int
      PL_LONG		    = 22;	 //* long
      PL_DOUBLE	    = 23;	 //* double
      PL_NCHARS	    = 24;	 //* size_t, const char *
      PL_UTF8_CHARS	= 25;	 //* const char *
      PL_UTF8_STRING= 26;	 //* const char *
      PL_INT64	    = 27;	 //* int64_t
      PL_NUTF8_CHARS= 28;	 //* size_t, const char *
      PL_NUTF8_CODES= 29;	 //* size_t, const char *
      PL_NUTF8_STRING=30;  //* size_t, const char *
      PL_NWCHARS	   =31;	 //* size_t, const wchar_t *
      PL_NWCODES	   =32;  //* size_t, const wchar_t *
      PL_NWSTRING	   =33;	 //* size_t, const wchar_t *
      PL_MBCHARS	   =34;	 //* const char *
      PL_MBCODES	   =35;	 //* const char *
      PL_MBSTRING	   =36;	 //* const char *
      PL_INTPTR	     =37;	 //* intptr_t
      PL_CHAR		     =38;	 //* int
      PL_CODE		     =39;	 //* int
      PL_BYTE		     =40;	 //* int

      (* PL_skip_list() *)

      PL_PARTIAL_LIST	 = 41;		//* a partial list
      PL_CYCLIC_TERM	 = 42;		//* a cyclic list/term
      PL_NOT_A_LIST	   = 43;		//* Object is not a list

					(* dicts *)

      PL_DICT		      = 44;

   (* Or'ed flags for PL_set_prolog_flag() *)

  (* MUST fit in a c short int - its a word in delphi! *)
  FF_READONLY	 : Word = $1000;		// Read-only prolog flag
  FF_KEEP		   : Word = $2000;		//* keep prolog flag if already set
  FF_NOCREATE	 : Word = $4000;		// Fail if flag is non-existent 9
  FF_FORCE	   : Word = $8000;		// Force setting, overwrite READONLY */
  FF_MASK		   : Word = $f000;


  (********************************
		*    DETERMINISTIC CALL/RETURN  *
		*********************************)

  // PL_succeed and PL_fail are deprecated
 	PL_succeed	= TRUE;  	//* succeed deterministically */
  PL_fail		  = FALSE;  //* fail */


  (******************************
	  NON-DETERMINISTIC CALL/RETURN *
	 *********************************)

 (*  Note 1: Non-deterministic foreign functions may also use the deterministic
    return methods PL_succeed and PL_fail.

    Note 2: The argument to PL_retry is a sizeof(ptr)-2 bits signed
    integer (use type intptr_t). *)

  PL_FIRST_CALL		= 0;
  PL_CUTTED		    = 1; //* deprecated */
  PL_PRUNED		    = 1; //
  PL_REDO		      = 2;
  PL_RESUME		    = 3;

type
  //PL_engine_t = ^PL_local_data; ???
  PL_engine_t = Pointer;

  queryRef = record
   engine : PL_engine_t;  // engine for the query
   offset : LongWord;      // queryFrane offset in local stack */
  end;

  QID_T = ^queryRef;
  //qid_t       = LongWord;       // opaque query handle


  int64_t     = int64;
  uInt64_t    = uint64;

  size_t      = Cardinal; // todo: check longword


  module_t    = Pointer;     // Prolog module
  predicate_t = Pointer;  // Prolog procedure
  record_t    = Pointer;     // Prolog recorded term

  { 32-bit platforms and 64-bit Windows platforms
    0..4294967295
    (0..232-1)   Unsigned 32-bit

   Cardinal
   64-bit POSIX platforms including iOS, macOS, Android, and Linux
   0..18446744073709551615
   (0..264-1) Unsigned 64-bit
 	UInt64 }

  atom_t      = LongWord;      // Prolog atom       (swi et yap)
  term_t      = LongWord;      // opaque term handle

  fid_t       = LongWord;       // opaque foreign context handle
  functor_t   = LongWord;   // Name/arity pair
  atomic_t    = LongWord;    // same as word
  control_t   = LongWord;   // non-deterministic control arg
  foreign_t   = LongWord;   // return type of foreign functions
// CPP:
  pl_function_t = Pointer; // can only pass function as void *
// C:
//  pl_function_t = function(): foreign_t; // foreign language functions

  atom_ta = array [0..10000] of atom_t ;
  pAtom_Ta = ^atom_ta;

  term_ta = array[0..10000] of term_t;
  pterm_ta = ^term_ta;

  (* Well, size_t, uintptr_t, intptr_t and ptrdiff_t type defined in C
  programming language really puzzles me.

  In real mode, size_t and ptrdiff_t are 16 bit types. The size of a pointer
  depends on whether it's a near, far, or huge pointer. Near pointers have
  16 bits, the other 32 bits. intptr_t and uintptr_t were only defined much
  later in C99.

  I noticed that on 32-bit systems using flat memory mode, the real size of
  above four types is:

  sizeof(uintptr_t) = sizeof(intptr_t) = sizeof(ptrdiff_t) = 4

  And on 64-bit systems using flat memory mode, the real size of above four
  types is:

  sizeof(uintptr_t) = sizeof(intptr_t) = sizeof(ptrdiff_t) = 8 *)

  intptr_t  = LongInt;
  uintptr_t = LongWord;

  buf_mark_t = uintptr_t	;	// buffer mark handle */


  {PL_retry(n)		return _PL_retry(n)
  PL_retry_address(a)	return _PL_retry_address(a)
  PL_yield_address(a)	return _PL_yield_address(a)}

  term_value_t = packed record
    case Integer of
      0: (i: Integer);   // PL_INTEGER
      1: (f: Double);    // PL_FLOAT
      2: (s: PAnsiChar);     // PL_STRING
      3: (a: atom_t);    // PL_ATOM
      4: (t: record name: atom_t; arity: Integer; end)  // PL_TERM
  end;

var

  //PL_EXPORT(foreign_t)	_PL_retry(intptr_t);
  _PL_retry          : function (p: LongInt): foreign_t;
  // PL_EXPORT(foreign_t)	_PL_retry_address(void *);
  _PL_retry_address  : function (a: Pointer): foreign_t;
  // PL_EXPORT(foreign_t)	_PL_yield_address(void *);

  PL_foreign_control : function (c: control_t): Integer;
  PL_foreign_context : function (c: control_t): LongInt;
  PL_foreign_context_address :
                       function (c: control_t): Pointer;
  PL_foreign_context_predicate :
                       function (c : control_t) : Pointer;

  (********************************
	 *      REGISTERING FOREIGNS     *
	 *********************************)

////////////////////////////////////////////////////////////////////////////////
// REGISTERING FOREIGNS
////////////////////////////////////////////////////////////////////////////////
type
  PL_extension = packed record
    predicate_name: PAnsiChar;             // Name of the predicate
    arity: SmallInt;                   // Arity of the predicate
    function_: pl_function_t;          // Implementing functions
    flags: SmallInt;                   // Or of PL_FA_...
  end;
const
  PL_FA_NOTRACE          = $01;	// foreign cannot be traced
  PL_FA_TRANSPARENT      = $02;	// foreign is module transparent
  PL_FA_NONDETERMINISTIC = $04;	// foreign is non-deterministic
  PL_FA_VARARGS          = $08;	// call using t0, ac, ctx

  PL_FA_CREF		         = $10; //* Internal: has clause-reference
  PL_FA_ISO	             = $20;	//* Internal: ISO core predicate
  PL_FA_META		         = $40; //* Additional meta-argument spec
  PL_FA_SIG_ATOMIC       = $80; //* Internal: do not dispatch signals

//extern PL_extension PL_extensions[]; /* not Win32! */
var //PL_EXPORT(void)		PL_register_extensions(const PL_extension *e);

    PL_register_extensions : procedure (var e: PL_extension);
    PL_load_extensions     : procedure (var e: PL_extension);
    PL_register_foreign    : function (name: PAnsiChar;
                             arity: Integer;
                             func: pl_function_t;
                             flags: Integer): Integer;
    PL_register_extensions_in_module : procedure
          (const module : PAnsiChar; var e : PL_extension);
 {PL_EXPORT(int)		PL_register_foreign_in_module(const char *module,
						      const char *name, int arity,
						      pl_function_t func,
						      int flags, ...);
 PL_EXPORT(void)		PL_load_extensions(const PL_extension *e); /* WDEPRECATED */ }

		 (*******************************
		 *	      LICENSE		*
		 *******************************)

   PL_license : procedure(license, module: PAnsiChar);

    // PL_license(const char *license, const char *module);

		(********************************
		*            MODULES            *
		*********************************)

////////////////////////////////////////////////////////////////////////////////
// MODULES
////////////////////////////////////////////////////////////////////////////////
///
  PL_context : function (): module_t;
  PL_module_name : function (module: module_t): atom_t;
  PL_new_module : function (name: atom_t): module_t;
  PL_strip_module : function (in_: term_t; var m: module_t; out_: term_t): Integer;

  (*******************************
	 *	     CONSTANTS		*
	 *******************************)

{PL_EXPORT(const atom_t) *_PL_atoms(void); /* base of reserved (meta-)atoms */
#ifndef PL_KERNEL
#define ATOM_nil	(_PL_atoms()[0]) /* `[]` */
#define ATOM_dot	(_PL_atoms()[1]) /* `.` */
#endif /*PL_KERNEL*/ }

////////////////////////////////////////////////////////////////////////////////
// CALL-BACK
////////////////////////////////////////////////////////////////////////////////
const
  PL_Q_DEBUG           = $01; // (!) = TRUE for backward compatibility
  PL_Q_NORMAL          = $02; // normal usage
  PL_Q_NODEBUG	        = $04; // use this one
  PL_Q_CATCH_EXCEPTION = $08; // handle exceptions in C
  PL_Q_PASS_EXCEPTION  = $10; // pass to parent environment
  PL_Q_ALLOW_YIELD	   = $0020;	//* Support I_YIELD
  PL_Q_EXT_STATUS		   = $0040;	//* Return extended status
  PL_Q_DETERMINISTIC   = $100; // (!) call was deterministic
	// PL_Q_EXT_STATUS return codes

  PL_S_NOT_INNER	     =  -2; //* Query is not inner query
  PL_S_EXCEPTION	     =  -1; //* Query raised exception
  PL_S_FALSE		       =   0; //* Query failed
  PL_S_TRUE		         =   1; //* Query succeeded with choicepoint
  PL_S_LAST		         =   2; //* Query succeeded without CP
  PL_S_YIELD	         = 255; //* Foreign yield

  // Foreign context frames

var PL_open_foreign_frame   : function (): fid_t;
    PL_rewind_foreign_frame : procedure (cid: fid_t);
    PL_close_foreign_frame  : procedure (cid: fid_t);
    PL_discard_foreign_frame: procedure (cid: fid_t);

  	// Finding predicates
    PL_pred      : function (f: functor_t; m: module_t): predicate_t;
    PL_predicate : function (name: PAnsiChar;
                      arity: Integer;
                      module: PAnsiChar): predicate_t;
    PL_predicate_info : function (pred: predicate_t;
                           var name: atom_t;
                           var arity: Integer;
                           var module: module_t): Integer;



		// Call-back
   PL_open_query : function(m : module_t; flags : Integer;
				      pred : predicate_t; t0 : term_t) : qid_t;

   //qid_t PL_open_query(module_t ctx, int flags, predicate_t p, term_t +t0)

   {PL_open_query : function (m: module_t;
                       flags: Integer;
                       const pred: predicate_t;
                       t0: term_t): qid_t; }

   PL_next_solution : function (var qid: qid_t): integer;

   PL_close_query : procedure (qid: qid_t);
   PL_cut_query : procedure (qid: qid_t);
   PL_current_query : function() : qid_t;
   PL_query_engine : function(qid : qid_t ) : PL_engine_t;
   PL_can_yield : procedure();

 	// Simplified (but less flexible) call-back */
   PL_call : function (t: term_t; m: module_t): Integer;
  //PL_EXPORT(int)		PL_call_predicate(module_t m, int flags,
	//				   predicate_t pred, term_t t0); /* TODO: WUNUSED */
  PL_call_predicate : function (const m: module_t;
                           const debug: Integer;
                           const pred: predicate_t;
                           t0: term_t): Integer;
  // Handling exceptions
  PL_exception : function (qid: qid_t): term_t;
  PL_raise_exception : function (exception: term_t): Integer;
  PL_throw : function (exception: term_t): Integer;
  PL_clear_exception : procedure();
  // PL_EXPORT(void)		PL_clear_exception(void);


  (* Engine-based coroutining *)
  PL_yielded : function (qid : qid_t): term_t;


 (*******************************
	*	      ASSERT		*
	*******************************)

const PL_ASSERTZ	            = $0000;
      PL_ASSERTA	            = $0001;
      PL_CREATE_THREAD_LOCAL	= $0010;
      PL_CREATE_INCREMENTAL	  = $0020;

//PL_EXPORT(int)		PL_assert(term_t term, module_t m, int flags);

var  PL_assert : function(term : term_t; m : module_t; flags : integer ) : integer;

(*******************************
 *        TERM-REFERENCES	*
 *******************************)

(* Creating and destroying term-refs *)
  PL_new_term_refs : function (n: Integer): term_t;
  PL_new_term_ref : function (): term_t;
  PL_copy_term_ref : function (from: term_t): term_t;
  PL_reset_term_refs : procedure (r: term_t);

 // Constants
 PL_new_atom    : function (s: PAnsiChar): atom_t;
 //PL_EXPORT(atom_t)	PL_new_atom_nchars(size_t len, const char *s);
 PL_new_atom_nchars : function (len: Cardinal; s: PAnsiChar): atom_t;
 PL_atom_chars  : function (a: atom_t): PAnsiChar;
 PL_atom_nchars : function (a: atom_t; var len: Cardinal): PAnsiChar;


 {todo:
  PL_new_atom_wchars : function(size_t len, const pl_wchar_t *s) : atom_t;
 PL_new_atom_mbchars : function(int rep, size_t len, const char *s) : atom_t;
 PL_atom_mbchars : function
  (atom_t a, size_t *len, char **s, unsigned int flags) : atom_t;

PL_EXPORT(const wchar_t *)	PL_atom_wchars(atom_t a, size_t *len); }

 PL_register_atom : procedure(a : atom_t );
 PL_unregister_atom : procedure(a : atom_t );

 {_PL_debug_register_atom : procedure(atom_t a,
						const char *file, int line,
						const char *func);
 PL_EXPORT(void)		_PL_debug_unregister_atom(atom_t a,
						  const char *file, int line,
						  const char *func); }
 PL_new_functor_sz : function(f : atom_t; a : cardinal) : functor_t;
 PL_new_functor : function( f : atom_t; a : Integer) : functor_t;
 PL_functor_name : function(f : functor_t): atom_t;

 //PL_EXPORT(int)		PL_functor_arity(functor_t f);
 PL_functor_arity : function(f : functor_t) : Integer;
 //PL_EXPORT(size_t)	PL_functor_arity_sz(functor_t f);
 PL_functor_arity_sz : function(f : functor_t ) : cardinal;

 // Get C(Pascal) values from Prolog terms
 PL_get_atom : function (t: term_t; var a: atom_t): Integer;
 PL_get_bool : function (t: term_t; var value: Integer): Integer;
 PL_get_atom_chars : function (t: term_t; var a: PAnsiChar): Integer;
 // TODO
//#define PL_get_string_chars(t, s, l) PL_get_string(t,s,l)
//					/* PL_get_string() is depreciated */
 PL_get_string : function (t: term_t; var s: PAnsiChar; var len: Cardinal): Integer;

 PL_get_chars : function (t: term_t; var s: PAnsiChar; flags: Cardinal): Integer;

 PL_get_list_chars : function (l: term_t; var s: PAnsiChar; flags: Cardinal): Integer;
 PL_get_atom_nchars : function (t: term_t; var length: Cardinal; var a: PAnsiChar): Integer;
 PL_get_list_nchars : function (l: term_t; var length: Cardinal; var s: PAnsiChar; flags: Cardinal): Integer;
 PL_get_nchars : function (t: term_t; var length: Cardinal; var s: PAnsiChar; flags: Cardinal): Integer;
 PL_get_integer : function (t: term_t; var i: Integer): Integer;
 PL_get_long : function (t: term_t; var i: LongInt): Integer;
 PL_get_pointer : function (t: term_t; var ptr: Pointer): Integer;
 PL_get_float : function (t: term_t; var f: double): Integer;
 PL_get_functor : function (t: term_t; var f: functor_t): Integer;
 PL_get_name_arity : function (t: term_t; var name: atom_t; var arity: Integer): Integer;
 PL_get_module : function (t: term_t; var module: module_t): Integer;
 PL_get_arg : function (index: Integer; t, a: term_t): Integer;

 PL_get_list : function (l, h, t: term_t): Integer;
 PL_get_head : function (l, h: term_t): Integer;
 PL_get_tail : function (l, t: term_t): Integer;
 PL_get_nil : function (l: term_t): Integer;
 PL_get_term_value : function (t: term_t; var v: term_value_t): Integer;
 PL_quote : function (chr: Integer; data: PAnsiChar): PAnsiChar;

// Verify types
 PL_term_type : function (t: term_t): Integer;

 PL_is_variable : function (t: term_t): Integer;
 PL_is_atom : function (t: term_t): Integer;
 PL_is_integer : function (t: term_t): Integer;
 PL_is_string : function (t: term_t): Integer;
 PL_is_float : function (t: term_t): Integer;
 PL_is_compound : function (t: term_t): Integer;
 PL_is_functor : function (t: term_t; f: functor_t): Integer;
 PL_is_list : function (t: term_t): Integer;
 PL_is_atomic : function (t: term_t): Integer;
 PL_is_number : function(t: term_t): Integer;
 (* Verify types *)
 PL_is_ground   : function (t: term_t ) : Integer;
 PL_is_rational : function (t : term_t) : Integer;

 PL_is_callable : function (t : term_t) : Integer;
 PL_is_dict     : function (t : term_t) : Integer;
 PL_is_pair     : function (t : term_t) : Integer;
 PL_is_acyclic  : function (t : term_t) : Integer;

// Assign to term-references
 // PL_EXPORT(int)
 PL_put_variable : procedure (t: term_t);

 PL_put_atom : procedure (t: term_t; a: atom_t);
 PL_put_atom_chars : procedure (t: term_t; chars: PAnsiChar);

 PL_put_string_chars : procedure (t: term_t; chars: PAnsiChar);
 PL_put_list_chars : procedure (t: term_t; chars: PAnsiChar);
 PL_put_list_codes : procedure (t: term_t; chars: PAnsiChar);

 PL_put_atom_nchars : procedure (t: term_t; l: Cardinal; chars: PAnsiChar);
 PL_put_string_nchars : procedure (t: term_t; len: Cardinal; chars: PAnsiChar);
 PL_put_list_nchars : procedure (t: term_t; l: Cardinal; chars: PAnsiChar);
 PL_put_list_ncodes : procedure (t: term_t; l: Cardinal; chars: PAnsiChar);
 PL_put_integer : procedure (t: term_t; i: LongInt);
 PL_put_pointer : procedure (t: term_t; ptr: Pointer);
 PL_put_float : procedure(t: term_t; f: double);
 PL_put_functor : procedure(t: term_t; functor: functor_t);
 PL_put_list : procedure (l: term_t);
 PL_put_nil : procedure (l: term_t);
 PL_put_term : procedure (t1, t2: term_t);

 PL_get_name_arity_sz :
    function(t : term_t;
             var name : atom_t; var arity : cardinal ): Integer;
PL_put_bool  : function(t : term_t; val : Integer) : Integer;
PL_put_chars : function (t : term_t; flags : integer;
				     len : Cardinal; {const} chars: PAnsiChar) : Integer;

PL_put_dict : function(t : term_t; tag : atom_t; len : Cardinal;
				    {const} keys : pAtom_Ta;  values : pterm_ta) : Integer;

PL_put_int64 : function(t : term_t; i : int64_t) : Integer;
PL_put_uint64 : function(t : term_t; i : uint64_t) : Integer;

_PL_put_atomic : procedure (t: term_t; a: atomic_t);

PL_put_term_from_chars : function
   (t : term_t; flags : Integer; len : Cardinal; {const} s : PAnsiChar) : Integer;

 { TODO:


PL_EXPORT(int)		PL_get_compound_name_arity_sz(term_t t, atom_t *name,
						      size_t *arity) WUNUSED}

PL_get_compound_name_arity : function
 (t : term_t; var name : atom_t; var arity : integer) : Integer;

PL_get_arg_sz : function
  (index : cardinal; t,a : term_t) : Integer;

PL_get_dict_key : function (key : atom_t; dict, value : term_t ) : Integer;

(*#define PL_FOR_DICT_SORTED	$1
PL_EXPORT(int)		PL_for_dict(term_t dict,
				    int (*func)(term_t key, term_t value, void *closure),
				    void *closure,
				    int flags); *)

//_PL_cons_small_int : function(v : int64_t) : atom_t;
//_PL_unregister_keys : procedure(len : cardinal; keys : pAtom_ta);

// construct a functor or list-cell */
// construct a functor or list-cell
// !!!!!! TODO
//procedure PL_cons_functor(h: term_t; f: functor_t; ...); cdecl; external 'libswipl.dll';
PL_cons_functor_v : function (h: term_t; fd: functor_t; a0: term_t) : integer;
PL_cons_list : function (l, h, t: term_t) : Integer;

// Unify term-references
PL_unify : function (t1, t2: term_t): Integer;
PL_unify_atom : function (t: term_t; a: atom_t): Integer;
PL_unify_atom_chars : function (t: term_t; chars: PAnsiChar): Integer;
PL_unify_list_chars : function (t: term_t; chars: PAnsiChar): Integer;
PL_unify_list_codes : function (t: term_t; chars: PAnsiChar): Integer;
PL_unify_string_chars : function (t: term_t; chars: PAnsiChar): Integer;
PL_unify_atom_nchars : function (t: term_t; l: Cardinal; s: PAnsiChar): Integer;
PL_unify_list_ncodes : function (t: term_t; l: Cardinal; s: PAnsiChar): Integer;
PL_unify_list_nchars : function (t: term_t; l: Cardinal; s: PAnsiChar): Integer;
PL_unify_string_nchars : function (t: term_t; len: Cardinal; chars: PAnsiChar): Integer;
PL_unify_integer : function (t: term_t; n: LongInt): Integer;
PL_unify_float : function (t: term_t; f: double): Integer;
PL_unify_pointer : function (t: term_t; ptr: Pointer): Integer;
PL_unify_functor : function (t: term_t; f: functor_t): Integer;
PL_unify_list : function (l, h, t: term_t): Integer;
PL_unify_nil : function (l: term_t): Integer;
PL_unify_arg : function (index: Integer; t, a: term_t): Integer;
PL_unify_compound : function(t : term_t; f : functor_t) : Integer;

PL_unify_arg_sz : function(index : cardinal; t, a : term_t) : integer;
//PL_unify_term : function(term_t t, ...) : Integer;
PL_unify_chars : function(t : term_t; flags : Integer;
				       len : cardinal; {const} s : PAnsiChar) : Integer;

(*******************************
 *	       LISTS		*
 *******************************)

PL_skip_list : function( list, tail : term_t; var len : cardinal) : Integer;


(*******************************
 *    WIDE CHARACTER VERSIONS	*
 *******************************)

//PL_put_wchars : function(t : term_t; tp : Integer;
//				      len : Cardinal; const s : pWideChar {pl_wchar_t *s}) : Integer;
PL_unify_wchars : function(t : term_t;
					    len : Cardinal; {const} s : pWideChar {pl_wchar_t *s}) : Integer;
PL_unify_wchars_diff : function(t, tail : term_t; tp : Integer;
					len : cardinal; {const pl_wchar_t *s} s : pWideChar) : Integer;
PL_get_wchars : function(l : term_t; var len : Cardinal;
              var s : pWideChar;
              flags : Integer)  : Integer;

PL_utf8_strlen: function({const char *s, size_t len}
                        s : pWideChar; len : Cardinal) : Integer;

(*******************************
 *	   WIDE INTEGERS	*
 *******************************)

PL_get_int64 : function (t : term_t; var i : int64_t) : Integer;
PL_get_uint64 : function(t : term_t; var i : uint64_t) : Integer;
PL_unify_int64 : function(t : term_t; value : int64_t) : Integer;
PL_unify_uint64 : function(t : term_t; value : uint64_t) : Integer;


(*******************************
 *     ATTRIBUTED VARIABLES	*
 *******************************)

PL_is_attvar : function(t : term_t) : Integer;
PL_get_attr : function(v, a : term_t) : Integer;


(*******************************
 *           TABLING            *
 *******************************)

//PL_get_delay_list : function(l : term_t) : Integer;


(*******************************
 *	      ERRORS		*
 *******************************)

PL_get_atom_ex : function(t : term_t; var a : atom_t) : Integer;
PL_get_integer_ex : function(t : term_t; var i : integer) : Integer;
PL_get_long_ex : function(t : term_t; var i : longInt) : Integer;
PL_get_int64_ex : function(t : term_t; var i : int64_t) : Integer;
PL_get_uint64_ex : function(t : term_t; var i : uint64_t) : Integer;

//PL_get_intptr_ex : function(t : term_t; intptr_t *i);
PL_get_intptr_ex : function(t : term_t; var i : pInteger) : Integer;
PL_get_size_ex : function(t : term_t; var i : cardinal) : Integer;
PL_get_bool_ex : function(t : term_t; var i : Integer) : Integer;
PL_get_float_ex : function(t : term_t; var f : double) : Integer;
PL_get_char_ex : function(t : term_t; var p : Integer; eof : Integer) : Integer;
PL_unify_bool_ex : function(t : term_t; val : Integer) : Integer;
PL_get_pointer_ex : function(t : term_t; var addrp : Pointer) : Integer;
PL_unify_list_ex : function(l,h,t : term_t) : Integer;
PL_unify_nil_ex : function(l : term_t) : Integer;

PL_get_list_ex : function(l, h, t : term_t) : Integer;
PL_get_nil_ex : function(l : term_t ) : Integer;

PL_instantiation_error : function(culprit : term_t ) : Integer;
PL_uninstantiation_error : function(culprit : term_t ) : Integer;
PL_representation_error : function
  ({const char *resource} resource : PAnsiChar) : Integer;
PL_type_error : function
  ({const char *expected,} expected : PAnsiChar; culprit : term_t ) : Integer;
PL_domain_error : function(
 //const char *expected,
 expected : PAnsiChar;
 culprit :term_t ) : Integer;

PL_existence_error : function(
 //const char *type,
 tpe : PAnsiChar;
 culprit : term_t ) : Integer;
PL_permission_error : function(
//const char *operation,
operation : PAnsiChar;
//const char *type,
tpe : PAnsiChar;
culprit : term_t ) : Integer;

PL_resource_error : function(
 //const char *resource
 resource : PAnsiChar ) : Integer;
//PL_syntax_error : function(const char *msg, IOSTREAM *in);

////////////////////////////////////////////////////////////////////////////////
// FILENAME SUPPORT
////////////////////////////////////////////////////////////////////////////////
const
  PL_FILE_ABSOLUTE     = $01;	// return absolute path
  PL_FILE_OSPATH       = $02;	// return path in OS notation
  PL_FILE_SEARCH       = $04;	// use file_search_path
  PL_FILE_EXIST        = $08;	// demand file to exist
  PL_FILE_READ         = $10;	// demand read-access
  PL_FILE_WRITE        = $20;	// demand write-access
  PL_FILE_EXECUTE      = $40;	// demand execute-access
  PL_FILE_NOERRORS     = $80;	// do not raise exceptions

var PL_get_file_name : function (n: term_t; var name: PAnsiChar; flags: Integer): Integer;
    PL_changed_cwd : procedure ();
    PL_cwd : function (): PAnsiChar;
   // PL_EXPORT(int)		PL_get_file_nameW(term_t n, wchar_t **name, int flags);


(******************************
 *	       BLOBS		*
 *******************************)

{const

  PL_BLOB_MAGIC_B	= $75293a00; //* Magic to validate a blob-type */
  PL_BLOB_VERSION = 1;      		//* Current version */

  PL_BLOB_MAGIC	  = PL_BLOB_MAGIC_B OR PL_BLOB_VERSION;

  PL_BLOB_UNIQUE	= $01;	 //* Blob content is unique
  PL_BLOB_TEXT	  = $02;	 //* blob contains text
  PL_BLOB_NOCOPY	= $04;	 //* do not copy the data
  PL_BLOB_WCHAR	  = $08;	 //* wide character string



PL_EXPORT(int)		PL_is_blob(term_t t, PL_blob_t **type);
PL_EXPORT(int)		PL_unify_blob(term_t t, void *blob, size_t len,
				      PL_blob_t *type);
PL_EXPORT(atom_t)	PL_new_blob(void *blob, size_t len, PL_blob_t *type);
PL_EXPORT(int)		PL_put_blob(term_t t, void *blob, size_t len,
				    PL_blob_t *type);
PL_EXPORT(int)		PL_get_blob(term_t t, void **blob, size_t *len,
				    PL_blob_t **type);

PL_EXPORT(void*)	PL_blob_data(atom_t a,
				     size_t *len,
				     struct PL_blob_t **type);
PL_EXPORT(int)		PL_free_blob(atom_t blob);

PL_EXPORT(void)		PL_register_blob_type(PL_blob_t *type);
PL_EXPORT(PL_blob_t*)	PL_find_blob_type(const char* name);
PL_EXPORT(int)		PL_unregister_blob_type(PL_blob_t *type); }



(*******************************
 *    QUINTUS/SICSTUS WRAPPER	*
 *******************************)


 PL_cvt_i_bool   : function (p : term_t; var c : Integer) : Integer; //* Note "int" because C has no "bool" */
 PL_cvt_i_char   : function (p : term_t; c : PAnsiChar): Integer;
 PL_cvt_i_schar  : function (p : term_t; var c : SmallInt): Integer;
 PL_cvt_i_uchar  : function (p : term_t; var c : Byte): Integer;

 PL_cvt_i_short  : function (p: term_t ; var s : SmallInt): Integer;
 PL_cvt_i_ushort : function (p : term_t; var s : Word): Integer;
 PL_cvt_i_int    : function (p : term_t; var c : Integer): Integer;
 PL_cvt_i_uint   : function (p : term_t; var c : Cardinal): Integer;
 PL_cvt_i_long   : function (p : term_t; var c : Integer): Integer;
 PL_cvt_i_ulong  : function (p : term_t; var c : Cardinal): Integer;
 //PL_cvt_i_llong  : function (p : term_t; long long *c): Integer;
 //PL_cvt_i_ullong : function (p : term_t; unsigned long long *c): Integer;

 PL_cvt_i_int32  : function (p : term_t; var c : Integer): Integer;
 PL_cvt_i_uint32 : function (p : term_t; var c : Cardinal): Integer;
 PL_cvt_i_int64  : function (p : term_t; var c : int64_t): Integer;
 PL_cvt_i_uint64 : function (p : term_t; var c : uint64_t): Integer;
 PL_cvt_i_size_t : function (p : term_t; var c : Cardinal): Integer;
 PL_cvt_i_float  : function (p : term_t; var c : double): Integer;
 PL_cvt_i_single : function (p : term_t; var c : Single): Integer;

 PL_cvt_i_string : function (p : term_t; var c : PAnsiChar): Integer;
 PL_cvt_i_codes  : function (p : term_t; var c : PAnsiChar): Integer;
 PL_cvt_i_atom   : function (p : term_t; var c : atom_t): Integer;
 PL_cvt_i_address: function (p : term_t; var c : Pointer): Integer;

 PL_cvt_o_int64  : function (c : int64_t; p : term_t): Integer;
 PL_cvt_o_float  : function (c : double;  p : term_t): Integer;
 PL_cvt_o_single : function (c : single;  p : term_t): Integer;

 PL_cvt_o_string : function ({const} c : PAnsiChar; p : term_t): Integer;
 PL_cvt_o_codes  : function ({const} c : PAnsiChar; p : term_t): Integer;
 PL_cvt_o_atom   : function (c : atom_t; p : term_t): Integer;
 PL_cvt_o_address: function (address : Pointer; p : term_t): Integer;

 PL_new_nil_ref : function() : term_t;

/// set/get encoding for PL_cvt_*_string() functions.
/// The default is UTF-8 (REP_UTF8)

 PL_cvt_encoding : function() : Integer;
 PL_cvt_set_encoding : function(enc : Integer) : Integer;
 SP_set_state : procedure(state : Integer);
 SP_get_state : function() : Integer;

//*
//*	     COMPARE		*

  PL_compare       : function(t1, t2 : term_t) : Integer;
  PL_same_compound : function(t1, t2 : term_t) : Integer;

(*		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

PL_EXPORT(int)		PL_warning(const char *fmt, ...) WPRINTF12;
PL_EXPORT(int)		PL_warningX(const char *fmt, ...);
PL_EXPORT(void)		PL_fatal_error(const char *fmt, ...) WPRINTF12; *)


////////////////////////////////////////////////////////////////////////////////
// RECORDED DATABASE
////////////////////////////////////////////////////////////////////////////////
PL_record           : function (term: term_t): record_t;
PL_recorded         : function (rec: record_t; term: term_t) : record_t;
PL_erase            : procedure(rec: record_t);
PL_duplicate_record : function(r : record_t) : record_t;
PL_record_external   : function (t: term_t; var size: Cardinal): PAnsiChar;
PL_recorded_external : function (rec: PAnsiChar; term: term_t): Integer;
PL_erase_external    : function (rec: PAnsiChar): Integer;

////////////////////////////////////////////////////////////////////////////////
// INTERNAL FUNCTIONS
////////////////////////////////////////////////////////////////////////////////
_PL_get_atomic : function (t: term_t): atomic_t;

_PL_unify_atomic : function (t: term_t; a: atomic_t): Integer;
//_PL_copy_atomic : procedure (t: term_t; a: atomic_t);
//function _PL_get_name_arity(t: term_t; var name: atom_t; var arity: Integer): Integer; cdecl; external 'libswipl.dll';
_PL_get_arg : procedure (index: Integer; t, a: term_t);

(******************************
 *	    CHAR BUFFERS	*
 *******************************)

const
        CVT_ATOM	    = $00000001;
        CVT_STRING	  = $00000002;
        CVT_LIST	    = $00000004;
        CVT_INTEGER	  = $00000008;
        CVT_RATIONAL	= $00000010;
        CVT_FLOAT	    = $00000020;
        CVT_VARIABLE	= $00000040;
        CVT_NUMBER	  = CVT_RATIONAL or CVT_FLOAT;
        CVT_ATOMIC	  = CVT_NUMBER or CVT_ATOM or CVT_STRING;
        CVT_WRITE	    = $00000080;
        CVT_WRITE_CANONICAL = $00000100;
        CVT_WRITEQ	  =  $00000200;
        CVT_ALL		    = CVT_ATOMIC or CVT_LIST;
        CVT_XINTEGER	= $00000400 or CVT_INTEGER;
        CVT_MASK	    = $00000fff;

        CVT_EXCEPTION	= $00001000; //* throw exception on error
        CVT_VARNOFAIL	= $00002000;	//* return 2 if argument is unbound

        BUF_DISCARDABLE	= $00000000; //* Store in single thread-local buffer
        BUF_STACK	      = $00010000; //* Store in stack of buffers
        BUF_MALLOC	    = $00020000; //* Store using PL_malloc()
        BUF_ALLOW_STACK	= $00040000; //* Allow pointer into (global) stack

        BUF_RING	      = BUF_STACK; //* legacy ring buffer

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Output   representation   for   PL_get_chars()     and    friends.   The
prepresentation type REP_FN is for   PL_get_file_name()  and friends. On
Windows we use UTF-8 which is translated   by the `XOS' layer to Windows
UNICODE file functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

        REP_ISO_LATIN_1	 = $00000000; //* output representation
        REP_UTF8	       = $00100000;
        REP_MB		       = $00200000;

{$IFDEF WINDOWS}
        REP_FN		       = REP_UTF8;
{$ELSE}
        REP_FN		       = REP_MB;
{$ENDIF}

        PL_DIFF_LIST	   = $01000000;	// PL_unify_chars() */


(******************************
		*         STRING BUFFERS       *
		*******************************)

var PL_mark_string_buffers              : procedure(var mark : buf_mark_t);
    PL_release_string_buffers_from_mark : procedure(mark : buf_mark_t );


(*******************************
		 *	  STREAM SUPPORT	*
		 *******************************/

					/* Make IOSTREAM known to Prolog */
#define PL_open_stream  PL_unify_stream	/* compatibility */
PL_EXPORT(int)		PL_unify_stream(term_t t, IOSTREAM *s);
PL_EXPORT(int)		PL_get_stream_handle(term_t t, IOSTREAM **s);
PL_EXPORT(int)		PL_get_stream(term_t t, IOSTREAM **s, int flags);
PL_EXPORT(int)		PL_get_stream_from_blob(atom_t a, IOSTREAM**s, int flags);
PL_EXPORT(IOSTREAM* )	PL_acquire_stream(IOSTREAM *s);
PL_EXPORT(int)		PL_release_stream(IOSTREAM *s);
PL_EXPORT(int)		PL_release_stream_noerror(IOSTREAM *s);
PL_EXPORT(IOSTREAM * )	PL_open_resource(module_t m,
					 const char *name,
					 const char *rc_class,
					 const char *mode);

PL_EXPORT(IOSTREAM * )*_PL_streams(void);	/* base of streams */
#ifndef PL_KERNEL
#define Suser_input     (_PL_streams()[0])
#define Suser_output    (_PL_streams()[1])
#define Suser_error     (_PL_streams()[2])
#define Scurrent_input  (_PL_streams()[3])
#define Scurrent_output (_PL_streams()[4])
#endif *)

const

      PL_WRT_QUOTED		          = $01; //* quote atoms
      PL_WRT_IGNOREOPS	        = $02; //* ignore list/operators
      PL_WRT_NUMBERVARS	        = $04; //* print $VAR(N) as a variable
      PL_WRT_PORTRAY		        = $08; //* call portray
      PL_WRT_CHARESCAPES	      = $10; //* Output ISO escape sequences
      PL_WRT_BACKQUOTED_STRING  = $20; //* Write strings as `...`

	    //* Write attributed variables */
      PL_WRT_ATTVAR_IGNORE	    = $040; //* Default: just write the var */
      PL_WRT_ATTVAR_DOTS	      = $080; //* Write as Var{...} */
      PL_WRT_ATTVAR_WRITE	      = $100; //* Write as Var{Attributes} */
      PL_WRT_ATTVAR_PORTRAY	    = $200; //* Use Module:portray_attrs/2 */
      PL_WRT_ATTVAR_MASK        = PL_WRT_ATTVAR_IGNORE or
                                  PL_WRT_ATTVAR_DOTS or
	                                PL_WRT_ATTVAR_WRITE or
	                                PL_WRT_ATTVAR_PORTRAY;
      PL_WRT_BLOB_PORTRAY	      = $400; //* Use portray for non-text blobs
      PL_WRT_NO_CYCLES	        = $800; //* Never emit @(Template,Subst)
      PL_WRT_NEWLINE		        = $2000; //* Add a newline
      PL_WRT_VARNAMES		        = $4000; //* Internal: variable_names(List)
      PL_WRT_BACKQUOTE_IS_SYMBOL= $8000; //* ` is a symbol char
      PL_WRT_DOTLISTS		        = $10000; //* Write lists as .(A,B)
      PL_WRT_BRACETERMS         = $20000; //* Write {A} as {}(A)
      PL_WRT_NODICT		          = $40000; //* Do not write dicts pretty
      PL_WRT_NODOTINATOM        = $80000; //* never write a.b unquoted
      PL_WRT_NO_LISTS		        = $100000; //* Do not write lists as [...]
      PL_WRT_RAT_NATURAL        = $200000; //* Write rationals as 1/3
      PL_WRT_CHARESCAPES_UNICODE= $400000; //* Use \uXXXX escapes
      PL_WRT_QUOTE_NON_ASCII	  = $800000; //* Quote atoms containing non-ascii
      PL_WRT_PARTIAL		        = $1000000; //* Partial output */
      PL_WRT_NO_CHARESCAPES	    = $2000000; //* Do not Output ISO escapes */

//PL_EXPORT(int)	PL_write_term(IOSTREAM *s,
//			     term_t term,
//			     int precedence,
//			     int flags);

					//* PL_ttymode() results */
      PL_NOTTY	               = 0; //* -tty in effect */
      PL_RAWTTY	               = 1; //* get_single_char/1 */
      PL_COOKEDTTY	           = 2; //* normal input */

{PL_EXPORT(int)		PL_ttymode(IOSTREAM *s); }

var

 PL_chars_to_term : function (chars: PAnsiChar; term: term_t): Integer;


// PL_wchars_to_term : function({const} pl_wchar_t *chars, term_t term);


		 (*******************************
		 *	    EMBEDDING		*
		 *******************************)

type    PPAnsiChar = ^PAnsiChar;

const   PL_CLEANUP_STATUS_MASK		   = $0ffff;
        PL_CLEANUP_NO_RECLAIM_MEMORY = $10000;
        PL_CLEANUP_NO_CANCEL		     = $20000;

        PL_CLEANUP_CANCELED	=  0;
        PL_CLEANUP_SUCCESS	=  1;
        PL_CLEANUP_FAILED   = -1;
        PL_CLEANUP_RECURSIVE= -2;

var

  //PL_initialise : function (argc: Integer; argv: array of PAnsiChar): Integer; cdecl;

  PL_initialise : function (argc: Integer; argv: PPAnsiChar): Integer; cdecl;
  PL_is_initialised : function (var argc: Integer; var argv: array of PAnsiChar): Integer;
  PL_cleanup : function (status: Integer): Integer;
  PL_halt : function (status: Integer) : Integer;
  PL_toplevel : function (): Integer;

// procedure PL_install_readline(); cdecl; external 'libswipl.dll';
// PL_EXPORT(int)		PL_winitialise(int argc, wchar_t **argv);
// PL_EXPORT(int)		PL_set_resource_db_mem(const unsigned char *data,
//					       size_t size);


(*******************************
 *	  DYNAMIC LINKING	*
 *******************************)

PL_dlopen  : function({const} fle : PAnsiChar; flags : integer) : Pointer;
PL_dlerror : function() : PAnsiChar;
PL_dlsym   : function(handle : pointer; symbol : PAnsiChar) : Pointer;
PL_dlclose : function(handle : Pointer) : Integer;


(*******************************
 *      INPUT/PROMPT/ETC	*
 *******************************)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: the functions in this section are   not  documented, as as yet not
adviced for public usage.  They  are   intended  to  provide an abstract
interface for the GNU readline  interface   as  defined  in the readline
package.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

// PL_dispatch() modes
const PL_DISPATCH_NOWAIT    = 0;		//* Dispatch only once
      PL_DISPATCH_WAIT      = 1;		//* Dispatch till input available
      PL_DISPATCH_INSTALLED = 2;		//* dispatch function installed?

var

PL_dispatch        : function(fd, wait : Integer) : Integer;
PL_add_to_protocol : procedure({const} buf : PAnsiChar; count : cardinal);
PL_prompt_string  : function(fd : Integer) : PAnsiChar;
PL_write_prompt : procedure(dowrite : Integer);
PL_prompt_next : procedure(fd : Integer);
PL_atom_generator : function({const} prefix : PAnsiChar; state : Integer) : PAnsiChar;

{PL_EXPORT(pl_wchar_t*)	PL_atom_generator_w(const pl_wchar_t *pref,
					    pl_wchar_t *buffer,
					    size_t buflen,
					    int state);}


(*******************************
 *	MEMORY ALLOCATION	*
 *******************************)

 ////////////////////////////////////////////////////////////////////////////////
// MEMORY ALLOCATION
////////////////////////////////////////////////////////////////////////////////
  PL_malloc : function (size: size_t): Pointer;
  PL_malloc_atomic : function (size: size_t): Pointer;
  PL_malloc_uncollectable : function(size: size_t): Pointer;
  PL_malloc_unmanaged : function(size: size_t): Pointer;
  PL_malloc_atomic_unmanaged  : function(size: size_t): Pointer;
  PL_realloc : function (mem: Pointer; size: size_t): Pointer;
  PL_free : procedure (mem: Pointer);
  PL_linger: procedure (mem: Pointer);


(********************************
 *             HOOKS		*
 ********************************)

 const
  PL_DISPATCH_INPUT   = 0; // There is input available
  PL_DISPATCH_TIMEOUT = 1; // Dispatch timeout
type
  PL_dispatch_hook_t = function(fd: Integer): PInteger;
  PL_abort_hook_t = function(): Pointer;
  PL_initialise_hook_t = function(argc: Integer; argv: array of PAnsiChar): Pointer;
  PL_async_hook_t = function(): Pointer; // Win32 only (O_ASYNC_HOOK)
  PL_agc_hook_t = function(a: atom_t): PInteger;

var

  PL_dispatch_hook : function (dh: PL_dispatch_hook_t): PL_dispatch_hook_t;
  PL_abort_hook : procedure (ah: PL_abort_hook_t);
  PL_initialise_hook : procedure(ih: PL_initialise_hook_t);
  PL_abort_unhook : function (ah: PL_abort_hook_t): Integer;
  //PL_async_hook : function (n: Cardinal; ah: PL_async_hook_t): PL_async_hook_t;
  PL_agc_hook : function (ah: PL_agc_hook_t): PL_agc_hook_t;

(*******************************
 *	      OPTIONS		*
 *******************************)

//typedef enum {
const
 _OPT_END  = -1;
  OPT_BOOL =  0;				// int

  OPT_INT   = 1;				//* int
  OPT_INT64 = 2;				//* int64_t
  OPT_UINT64= 3;				// uint64_t
  OPT_SIZE  = 4;				// size_t
  OPT_DOUBLE= 5;				// double */
  OPT_STRING= 6;				// char* (UTF-8)
  OPT_ATOM  = 7;				// atom_t
  OPT_TERM  = 8;				// term_t */
  OPT_LOCALE= 9;				// void* */
//} _PL_opt_enum_t;

  OPT_TYPE_MASK	= $ff;
  OPT_INF		    = $100;		// allow 'inf' */

  OPT_ALL		    = $1;	  	//* flags */

(*
typedef struct
{ atom_t		name;		/* Name of option */
  _PL_opt_enum_t	type;		/* Type of option */
  const char *		string;		/* For foreign access */
} PL_option_t;

#define PL_OPTION(name, type) { 0, type, name }
#define PL_OPTIONS_END	      { 0, _OPT_END, (const char* )0 }

//PL_EXPORT(int)	PL_scan_options(term_t options, int flags, const char *opttype,
//				PL_option_t specs[], ...); *)


(********************************
 *            SIGNALS            *
 *********************************)

 //TODO

(* PL_signal() masks (deprecated) */
#define PL_SIGSYNC	$00010000	/* call handler synchronously */
#define PL_SIGNOFRAME	$00020000	/* Do not create a Prolog frame */

#define PLSIG_THROW     $0002		/* throw signal(num, name) */
#define PLSIG_SYNC      $0004		/* call synchronously */
#define PLSIG_NOFRAME   $0008		/* Do not create a Prolog frame */
#define PLSIG_IGNORE    $0010		/* ignore signal entirely */


typedef struct pl_sigaction
{ void        (*sa_cfunction)(int);	/* traditional C function */
  predicate_t sa_predicate;		/* call a predicate */
  int	      sa_flags;			/* additional flags */
  void       *reserved[2];		/* future extentions */
} pl_sigaction_t;


PL_EXPORT(void) (*PL_signal(int sig, void (*func)(int)))(int); /* WDEPRECATED */
PL_EXPORT(int)  PL_sigaction(int sig, pl_sigaction_t *act, pl_sigaction_t *old);
PL_EXPORT(void)	PL_interrupt(int sig);
PL_EXPORT(int)	PL_raise(int sig);
PL_EXPORT(int)	PL_handle_signals(void);
PL_EXPORT(int)	PL_get_signum_ex(term_t sig, int *n);  *)


(*********************************
 *      PROLOG ACTION/QUERY      *
 *********************************)

    PL_ACTION_TRACE		         = 1;	//* switch to trace mode
    PL_ACTION_DEBUG		         = 2;	//* switch to debug mode
    PL_ACTION_BACKTRACE	       = 3;  //* show a backtrace (stack dump)
    PL_ACTION_BREAK		         = 4;  //* create a break environment
    PL_ACTION_HALT		         = 5;  //* halt Prolog execution
    PL_ACTION_ABORT		         = 6;  //* generate a Prolog abort

		//* 7: Obsolete PL_ACTION_SYMBOLFILE

    PL_ACTION_WRITE		         = 8;  //* write via Prolog i/o buffer
    PL_ACTION_FLUSH		         = 9; 	//* Flush Prolog i/o buffer
    PL_ACTION_GUIAPP	         = 10; //* Win32: set when this is a gui
    PL_ACTION_ATTACH_CONSOLE   = 11; //* MT: Attach a console
    PL_GMP_SET_ALLOC_FUNCTIONS = 12; //* GMP: do not change allocation functions
    PL_ACTION_TRADITIONAL	     = 13; //* Set --traditional

    PL_BT_SAFE		             = $1; //* Do not try to print goals
    PL_BT_USER		             = $2; //* Only show user-goals

// todo: PL_EXPORT(int)	PL_action(int, ...);	/* perform some action */
{
var
  PL_on_halt   : procedure(int (*)(int, void *), void *);
  PL_exit_hook : procedure(int (*)(int, void *), void *);
  PL_backtrace : procedure(int depth, int flags);
PL_EXPORT(char *) PL_backtrace_string(int depth, int flags);
PL_EXPORT(int)	PL_check_data(term_t data);
PL_EXPORT(int)	PL_check_stacks(void);
PL_EXPORT(int)	PL_current_prolog_flag(atom_t name, int type, void *ptr); }


(*******************************
 *	      VERSIONS		*
 *******************************)

    PL_VERSION_SYSTEM	  = 1; //* Prolog version
    PL_VERSION_FLI		  = 2; //* PL_* compatibility
    PL_VERSION_REC		  = 3; //* PL_record_external() compatibility
    PL_VERSION_QLF	   	= 4; //* Saved QLF format version
    PL_VERSION_QLF_LOAD	= 5; //* Min loadable QLF format version
    PL_VERSION_VM		    = 6; //* VM signature
    PL_VERSION_BUILT_IN	= 7; //* Built-in predicate signature

// #define PL_version(id) PL_version_info(id)
var
    PL_version_info : function(which : Integer) : Integer;


(********************************
 *         QUERY PROLOG          *
 *********************************)

const

  PL_QUERY_ARGC		          = 1; //* return main() argc
  PL_QUERY_ARGV		          = 2; //* return main() argv
                         				 //* 3: Obsolete PL_QUERY_SYMBOLFILE
				                         //* 4: Obsolete PL_QUERY_ORGSYMBOLFILE
  PL_QUERY_GETC		          = 5; //* Read character from terminal
  PL_QUERY_MAX_INTEGER	    = 6; //* largest integer
  PL_QUERY_MIN_INTEGER	    = 7; //* smallest integer
  PL_QUERY_MAX_TAGGED_INT	  = 8; //* largest tagged integer
  PL_QUERY_MIN_TAGGED_INT	  = 9; //* smallest tagged integer
  PL_QUERY_VERSION          = 10; //* 207006 = 2.7.6
  PL_QUERY_MAX_THREADS	    = 11; //* maximum thread count
  PL_QUERY_ENCODING	        = 12; //* I/O encoding
  PL_QUERY_USER_CPU	        = 13; //* User CPU in milliseconds
  PL_QUERY_HALTING	        = 14; //* If TRUE, we are in PL_cleanup()

  PL_THREAD_NO_DEBUG	      = $01; //* Start thread in nodebug mode
  PL_THREAD_NOT_DETACHED	  = $02; //* Allow Prolog to join

var

  PL_query : function(info : Integer) : intptr_t;	//* get information from Prolog

(*******************************
 *	  PROLOG THREADS	*
 *******************************)

//typedef enum {
const
  PL_THREAD_CANCEL_FAILED    = 0; //FALSE,	/* failed to cancel; try abort */
  PL_THREAD_CANCEL_JOINED    = 1; //TRUE,	/* cancelled and joined */
  PL_THREAD_CANCEL_MUST_JOIN = 2;	//* cancelled, must join */
//} rc_cancel;

type

  PL_thread_attr_t = packed record
    stack_limit    : size_t;
    table_space    : size_t;
    alias          : PAnsiChar;
    rc_cancel      : pointer; // (*cancel)(int id);		/* cancel function */
    flags          : intptr_t;
    max_queue_size : size_t;
    reserved       : array[0..2] of Pointer; // void * reserved[3]; /* reserved for extensions
  end;

  PL_thread_exit_func_t = procedure;

var

  PL_thread_self : function (): Integer; // Prolog thread id (-1 if none)
  PL_unify_thread_id : function (t : term_t; i : Integer) : Integer;
  PL_get_thread_id_ex : function(t : term_t; var idp : Integer) : Integer;
  PL_get_thread_alias : function (tid : Integer; var alias : atom_t) : Integer;
  PL_thread_attach_engine : function (var attr : PL_thread_attr_t) : Integer;
  PL_thread_destroy_engine : function() : Integer;
  PL_thread_at_exit : function
    (function_: PL_thread_exit_func_t; closure: Pointer; global: Integer): Integer;
  PL_thread_raise : function(tid, sig : Integer) : Integer;
{$IFDEF WINDOWS}
  PL_w32thread_raise : function (dwTid: LongWord; sig: Integer): Integer;
  PL_wait_for_console_input : function(handle : Pointer) : Integer;
  PL_w32_wrap_ansi_console : function() : Integer;
  PL_w32_running_under_wine: function() : PAnsiChar;
{$ENDIF}

(*******************************
 *	 ENGINES (MT-ONLY)	*
 *******************************)
 const  PL_ENGINE_MAIN	   : Integer = $1;
        PL_ENGINE_CURRENT  : Integer = $2;

        PL_ENGINE_SET   = 0; //* engine set successfully */
        PL_ENGINE_INVAL	= 2; //* engine doesn't exist */
        PL_ENGINE_INUSE	= 3; //* engine is in use */

 var

    PL_create_engine : function(var attributes : PL_thread_attr_t) : Pointer;
    PL_set_engine : function(engine : PL_engine_t; var old : PL_engine_t) : Integer;
    PL_destroy_engine : function(engine : PL_engine_t) : Integer;


(*******************************
 *	    HASH TABLES		*
 *******************************

typedef _PLS(table)	  *hash_table_t;
typedef _PLS(table_enum) *hash_table_enum_t;

#define PL_HT_NEW	$0001
#define PL_HT_UPDATE	$0002

PL_EXPORT(hash_table_t)	PL_new_hash_table(int size,
					  void (*free_symbol)(void *n, void *v));
PL_EXPORT(int)		PL_free_hash_table(hash_table_t table);
PL_EXPORT(void* )	PL_lookup_hash_table(hash_table_t table, void *key);
PL_EXPORT(void* )	PL_add_hash_table(hash_table_t table,
					  void *key, void *value, int flags);
PL_EXPORT(void* )	PL_del_hash_table(hash_table_t table, void *key);
PL_EXPORT(int)		PL_clear_hash_table(hash_table_t table);
PL_EXPORT(hash_table_enum_t) PL_new_hash_table_enum(hash_table_t table);
PL_EXPORT(void)		PL_free_hash_table_enum(hash_table_enum_t e);
PL_EXPORT(int)		PL_advance_hash_table_enum(hash_table_enum_t e,
						   void **key, void **value); *)


(*******************************
 *	     PROFILER		*
 *******************************

typedef struct
{ int	(*unify)(term_t t, void *handle);	/* implementation --> Prolog */
  int   (*get)(term_t t, void **handle);	/* Prolog --> implementation */
  void	(*activate)(int active);		/* (de)activate */
  void  (*release)(void *handle);		/* Release handle */
  void *dummy[4];				/* reserved */
  intptr_t	magic;				/* PROFTYPE_MAGIC */
} PL_prof_type_t;

PL_EXPORT(int)		PL_register_profile_type(PL_prof_type_t *type);
PL_EXPORT(void*c)	PL_prof_call(void *handle, PL_prof_type_t *type);
PL_EXPORT(void)		PL_prof_exit(void *node);


(*******************************
 *	      DEBUG		*
 *******************************

PL_EXPORT(int)		PL_prolog_debug(const char *topic);
PL_EXPORT(int)		PL_prolog_nodebug(const char *topic);c* )


		 /*******************************
		 *	 WINDOWS MESSAGES	*
		 *******************************/

#if defined(_WINDOWS_) || defined(_WINDOWS_H)	/* <windows.h> is included */
#define PL_MSG_EXCEPTION_RAISED -1
#define PL_MSG_IGNORED 0
#define PL_MSG_HANDLED 1

PL_EXPORT(LRESULT)	PL_win_message_proc(HWND hwnd,
					    UINT message,
					    WPARAM wParam,
					    LPARAM lParam);
#endif /* _WINDOWS_/_WINDOWS_H */


		 /*******************************
		 *       FAST XPCE SUPPORT	*
		 *******************************/

typedef struct
{ int type;				/* PL_INTEGER or PL_ATOM */
  union
  { uintptr_t i;			/* integer reference value */
    atom_t	  a;			/* atom reference value */
  } value;
} xpceref_t;

PL_EXPORT(int)	_PL_get_xpce_reference(term_t t, xpceref_t *ref);
PL_EXPORT(int)	_PL_unify_xpce_reference(term_t t, xpceref_t *ref);
PL_EXPORT(int)	_PL_put_xpce_reference_i(term_t t, uintptr_t r);
PL_EXPORT(int)	_PL_put_xpce_reference_a(term_t t, atom_t name);



		 /*******************************
		 *         TRACE SUPPORT	*
		 *******************************/

typedef struct pl_context_t
{ PL_engine_t   ld;			/* Engine */
  _PLS(queryFrame) *qf;			/* Current query */
  _PLS(localFrame) *fr;			/* Current localframe */
  _PLQ(code) *	pc;			/* Code pointer */
  void *	reserved[10];		/* Reserved for extensions */
} pl_context_t;

PL_EXPORT(int)	PL_get_context(struct pl_context_t *c, int thead_id);
PL_EXPORT(int)	PL_step_context(struct pl_context_t *c);
PL_EXPORT(int)	PL_describe_context(struct pl_context_t *c,
				    char *buf, size_t len);

/* Define as 1 if undefined or defined as empty */
#if !defined(PL_ARITY_AS_SIZE) || (0-PL_ARITY_AS_SIZE-1)==1
#undef PL_ARITY_AS_SIZE
#define PL_ARITY_AS_SIZE 1
#endif

#if PL_ARITY_AS_SIZE
#define PL_new_functor(f,a) PL_new_functor_sz(f,a)
#define PL_functor_arity(f) PL_functor_arity_sz(f)
#define PL_get_name_arity(t,n,a) PL_get_name_arity_sz(t,n,a)
#define PL_get_compound_name_arity(t,n,a) PL_get_compound_name_arity_sz(t,n,a)
#define PL_get_arg(i,t,a) PL_get_arg_sz(i,t,a)
#define PL_unify_arg(i,t,a) PL_unify_arg_sz(i,t,a)
#define _PL_get_arg(i,t,a) _PL_get_arg_sz(i,t,a)
#else
#warning "Term arity has changed from int to size_t."
#warning "Please update your code or use #define PL_ARITY_AS_SIZE 0."
#endif




#ifdef __GNU_MP__

		 /*******************************
		 *	       GMP		*
		 *******************************/

PL_EXPORT(int)	PL_get_mpz(term_t t, mpz_t mpz) : Integer;
PL_EXPORT(int)	PL_get_mpq(term_t t,  mpq_t mpq) : Integer;
PL_EXPORT(int)	PL_unify_mpz(term_t t, mpz_t mpz) : Integer;
PL_EXPORT(int)	PL_unify_mpq(term_t t, mpq_t mpq) : Integer;

#endif /*__GNU_MP__*/
*)

function CHCK(Value : Integer) : Boolean; inline;

procedure LoadSWIProlog(const P: String; raiseError : Boolean = True);

function Loaded : Boolean;
procedure Reset;


procedure PL_STRINGS_MARK(var mark : buf_mark_t);     inline;
procedure PL_STRINGS_RELEASE(var mark : buf_mark_t);  inline;


implementation
uses SysUtils,
  Math,
  Windows;

function CHCK(Value : Integer) : Boolean;
begin
  result := Value <> 0;
end;

var
  Handle: HModule = 0;
  module: String = '';

procedure Reset;
begin
  Handle := 0;
  module := '';
end;

function Loaded : Boolean;
begin
  result := Handle <> 0;
end;

function ProcAddress(h: HModule; lpProcName: LPCWSTR): FarProc;
begin
  result := GetProcAddress(h, lpProcName);
  if result = nil then
    raise Exception.Create(lpProcName + ' not in module ' + module);
end;

procedure PL_STRINGS_MARK; inline;
begin
 PL_mark_string_buffers(mark);
end;

procedure PL_STRINGS_RELEASE; inline;
begin
 PL_release_string_buffers_from_mark(mark);
end;

procedure LoadSWIProlog(const P: String; raiseError : Boolean = True);
var
  ret: Integer;
  err: String;
begin
  // Laden nur wenn noch nicht geladen
  if Handle <> 0 then
    exit;

  module := P;
  Handle := SafeLoadLibrary(P,SEM_FAILCRITICALERRORS);
  if Handle = 0 then
  begin
    ret := GetLastError();
    err := SysErrorMessage(ret);
    if raiseError then
       raise Exception.Create(err)
    else
       Writeln(err);
  end; // ...

  PL_register_extensions  := ProcAddress(Handle, 'PL_register_extensions');
  PL_load_extensions  := ProcAddress(Handle, 'PL_load_extensions');
  PL_register_foreign := ProcAddress(Handle, 'PL_register_foreign');
  PL_register_extensions_in_module :=
          ProcAddress(Handle, 'PL_register_extensions_in_module');
  PL_license := ProcAddress(Handle, 'PL_license');
    // MODULES
  PL_context  := ProcAddress(Handle, 'PL_context');
  PL_module_name  := ProcAddress(Handle, 'PL_module_name');
  PL_new_module  := ProcAddress(Handle, 'PL_new_module');
  PL_strip_module  := ProcAddress(Handle, 'PL_strip_module');
    // Foreign context frames
  PL_open_foreign_frame  := ProcAddress(Handle, 'PL_open_foreign_frame');
  PL_rewind_foreign_frame  := ProcAddress(Handle, 'PL_rewind_foreign_frame');
  PL_close_foreign_frame  := ProcAddress(Handle, 'PL_close_foreign_frame');
  PL_discard_foreign_frame  := ProcAddress(Handle, 'PL_discard_foreign_frame');
  	// Finding predicates
  PL_pred  := ProcAddress(Handle, 'PL_pred');
  PL_predicate  := ProcAddress(Handle, 'PL_predicate');
  PL_predicate_info  := ProcAddress(Handle, 'PL_predicate_info');
  // Call-back
  PL_open_query  := ProcAddress(Handle, 'PL_open_query');
  PL_next_solution  := ProcAddress(Handle, 'PL_next_solution');
  PL_close_query  := ProcAddress(Handle, 'PL_close_query');
  PL_cut_query  := ProcAddress(Handle, 'PL_cut_query');
  PL_current_query  := ProcAddress(Handle, 'PL_current_query');
  PL_query_engine  := ProcAddress(Handle, 'PL_query_engine');

   	// Simplified (but less flexible) call-back */
  PL_call := ProcAddress(Handle, 'PL_call');
  PL_call_predicate  := ProcAddress(Handle, 'PL_call_predicate');
  // Handling exceptions
  PL_exception  := ProcAddress(Handle, 'PL_exception');
  PL_raise_exception  := ProcAddress(Handle, 'PL_raise_exception');
  PL_throw  := ProcAddress(Handle, 'PL_throw');
  PL_clear_exception  := ProcAddress(Handle, 'PL_clear_exception');
    // Engine-based coroutining
  PL_yielded  := ProcAddress(Handle, 'PL_yielded');
    //  ASSERT
  PL_assert  := ProcAddress(Handle, 'PL_assert');
  // TERM-REFERENCES
    (* Creating and destroying term-refs *)
  PL_new_term_refs  := ProcAddress(Handle, 'PL_new_term_refs');
  PL_new_term_ref  := ProcAddress(Handle, 'PL_new_term_ref');
  PL_copy_term_ref  := ProcAddress(Handle, 'PL_copy_term_ref');
  PL_reset_term_refs  := ProcAddress(Handle, 'PL_reset_term_refs');
  // Constants
  PL_new_atom  := ProcAddress(Handle, 'PL_new_atom');
  PL_new_atom_nchars  := ProcAddress(Handle, 'PL_new_atom_nchars');
  PL_atom_chars  := ProcAddress(Handle, 'PL_atom_chars');
  PL_atom_nchars  := ProcAddress(Handle, 'PL_atom_nchars');
  PL_register_atom  := ProcAddress(Handle, 'PL_register_atom');
  PL_unregister_atom  := ProcAddress(Handle, 'PL_unregister_atom');
  PL_new_functor_sz  := ProcAddress(Handle, 'PL_new_functor_sz');
  PL_new_functor  := ProcAddress(Handle, 'PL_new_functor');
  PL_functor_name  := ProcAddress(Handle, 'PL_functor_name');
  PL_functor_arity  := ProcAddress(Handle, 'PL_functor_arity');
  PL_functor_arity_sz  := ProcAddress(Handle, 'PL_functor_arity_sz');
  PL_get_atom  := ProcAddress(Handle, 'PL_get_atom');
  PL_get_bool  := ProcAddress(Handle, 'PL_get_bool');
  PL_get_atom_chars  := ProcAddress(Handle, 'PL_get_atom_chars');
  PL_get_string  := ProcAddress(Handle, 'PL_get_string');
  PL_get_chars  := ProcAddress(Handle, 'PL_get_chars');
  PL_get_list_chars  := ProcAddress(Handle, 'PL_get_list_chars');
  PL_get_atom_nchars  := ProcAddress(Handle, 'PL_get_atom_nchars');
  PL_get_list_nchars  := ProcAddress(Handle, 'PL_get_list_nchars');
  PL_get_nchars  := ProcAddress(Handle, 'PL_get_nchars');
  PL_get_integer  := ProcAddress(Handle, 'PL_get_integer');
  PL_get_long  := ProcAddress(Handle, 'PL_get_long');
  PL_get_pointer  := ProcAddress(Handle, 'PL_get_pointer');
  PL_get_float  := ProcAddress(Handle, 'PL_get_float');
  PL_get_functor  := ProcAddress(Handle, 'PL_get_functor');
  PL_get_name_arity  := ProcAddress(Handle, 'PL_get_name_arity');
  PL_get_module  := ProcAddress(Handle, 'PL_get_module');
  PL_get_arg  := ProcAddress(Handle, 'PL_get_arg');
  PL_get_list  := ProcAddress(Handle, 'PL_get_list');
  PL_get_head  := ProcAddress(Handle, 'PL_get_head');
  PL_get_tail  := ProcAddress(Handle, 'PL_get_tail');
  PL_get_nil  := ProcAddress(Handle, 'PL_get_nil');
  PL_get_term_value  := ProcAddress(Handle, 'PL_get_term_value');
  PL_quote  := ProcAddress(Handle, 'PL_quote');
  // Verify types
  PL_term_type  := ProcAddress(Handle, 'PL_term_type');
  PL_is_variable  := ProcAddress(Handle, 'PL_is_variable');
  PL_is_atom  := ProcAddress(Handle, 'PL_is_atom');
  PL_is_integer  := ProcAddress(Handle, 'PL_is_integer');
  PL_is_string  := ProcAddress(Handle, 'PL_is_string');
  PL_is_float  := ProcAddress(Handle, 'PL_is_float');
  PL_is_compound  := ProcAddress(Handle, 'PL_is_compound');
  PL_is_functor  := ProcAddress(Handle, 'PL_is_functor');
  PL_is_list  := ProcAddress(Handle, 'PL_is_list');
  PL_is_atomic  := ProcAddress(Handle, 'PL_is_atomic');
  PL_is_number  := ProcAddress(Handle, 'PL_is_number');
  PL_is_ground  := ProcAddress(Handle, 'PL_is_ground');
  PL_is_rational  := ProcAddress(Handle, 'PL_is_rational');
  PL_is_callable  := ProcAddress(Handle, 'PL_is_callable');
  PL_is_dict  := ProcAddress(Handle, 'PL_is_dict');
  PL_is_pair  := ProcAddress(Handle, 'PL_is_pair');
  PL_is_acyclic  := ProcAddress(Handle, 'PL_is_acyclic');

  PL_put_variable  := ProcAddress(Handle, 'PL_put_variable');
  PL_put_atom  := ProcAddress(Handle, 'PL_put_atom');
  PL_put_atom_chars  := ProcAddress(Handle, 'PL_put_atom_chars');
  PL_put_string_chars  := ProcAddress(Handle, 'PL_put_string_chars');
  PL_put_list_chars  := ProcAddress(Handle, 'PL_put_list_chars');
  PL_put_list_codes  := ProcAddress(Handle, 'PL_put_list_codes');
  PL_put_atom_nchars  := ProcAddress(Handle, 'PL_put_atom_nchars');
  PL_put_string_nchars  := ProcAddress(Handle, 'PL_put_string_nchars');
  PL_put_list_nchars  := ProcAddress(Handle, 'PL_put_list_nchars');
  PL_put_list_ncodes  := ProcAddress(Handle, 'PL_put_list_ncodes');
  PL_put_integer  := ProcAddress(Handle, 'PL_put_integer');
  PL_put_pointer  := ProcAddress(Handle, 'PL_put_pointer');
  PL_put_float  := ProcAddress(Handle, 'PL_put_float');
  PL_put_functor  := ProcAddress(Handle, 'PL_put_functor');
  PL_put_list  := ProcAddress(Handle, 'PL_put_list');
  PL_put_nil := ProcAddress(Handle, 'PL_put_nil');
  PL_put_term := ProcAddress(Handle, 'PL_put_term');
  PL_get_name_arity_sz  := ProcAddress(Handle, 'PL_get_name_arity_sz');
  PL_put_bool  := ProcAddress(Handle, 'PL_put_bool');
  PL_put_chars  := ProcAddress(Handle, 'PL_put_chars');
  PL_get_compound_name_arity  := ProcAddress(Handle, 'PL_get_compound_name_arity');
  PL_get_arg_sz  := ProcAddress(Handle, 'PL_get_arg_sz');
  PL_get_dict_key  := ProcAddress(Handle, 'PL_get_dict_key');
  PL_put_dict  := ProcAddress(Handle, 'PL_put_dict');
  //_PL_unregister_keys  := ProcAddress(Handle, '_PL_unregister_keys');
  PL_cons_functor_v  := ProcAddress(Handle, 'PL_cons_functor_v');
  PL_cons_list  := ProcAddress(Handle, 'PL_cons_list');
  // Unify term-references
  PL_unify  := ProcAddress(Handle, 'PL_unify');
  PL_unify_atom  := ProcAddress(Handle, 'PL_unify_atom');
  PL_unify_atom_chars  := ProcAddress(Handle, 'PL_unify_atom_chars');
  PL_unify_list_chars  := ProcAddress(Handle, 'PL_unify_list_chars');
  PL_unify_list_codes  := ProcAddress(Handle, 'PL_unify_list_codes');
  PL_unify_string_chars  := ProcAddress(Handle, 'PL_unify_string_chars');
  PL_unify_atom_nchars  := ProcAddress(Handle, 'PL_unify_atom_nchars');
  PL_unify_list_ncodes  := ProcAddress(Handle, 'PL_unify_list_ncodes');
  PL_unify_list_nchars  := ProcAddress(Handle, 'PL_unify_list_nchars');
  PL_unify_string_nchars  := ProcAddress(Handle, 'PL_unify_string_nchars');
  PL_unify_integer  := ProcAddress(Handle, 'PL_unify_integer');
  PL_unify_float  := ProcAddress(Handle, 'PL_unify_float');
  PL_unify_pointer  := ProcAddress(Handle, 'PL_unify_pointer');
  PL_unify_functor  := ProcAddress(Handle, 'PL_unify_functor');
  PL_unify_list  := ProcAddress(Handle, 'PL_unify_list');
  PL_unify_nil  := ProcAddress(Handle, 'PL_unify_nil');
  PL_unify_arg  := ProcAddress(Handle, 'PL_unify_arg');
  PL_unify_compound := ProcAddress(Handle, 'PL_unify_compound');
  PL_unify_arg_sz  := ProcAddress(Handle, 'PL_unify_arg_sz');
  PL_unify_chars  := ProcAddress(Handle, 'PL_unify_chars');
  PL_skip_list  := ProcAddress(Handle, 'PL_skip_list');
  //PL_put_wchars  := ProcAddress(Handle, 'PL_put_wchars');
  PL_unify_wchars  := ProcAddress(Handle, 'PL_unify_wchars');
  PL_unify_wchars_diff  := ProcAddress(Handle, 'PL_unify_wchars_diff');
  PL_get_wchars  := ProcAddress(Handle, 'PL_get_wchars');
  PL_utf8_strlen  := ProcAddress(Handle, 'PL_utf8_strlen');
  PL_get_int64  := ProcAddress(Handle, 'PL_get_int64');
  PL_get_uint64  := ProcAddress(Handle, 'PL_get_uint64');
  PL_unify_int64  := ProcAddress(Handle, 'PL_unify_int64');
  PL_unify_uint64  := ProcAddress(Handle, 'PL_unify_uint64');
  PL_put_int64  := ProcAddress(Handle, 'PL_put_int64');
  PL_put_uint64  := ProcAddress(Handle, 'PL_put_uint64');
  //*     ATTRIBUTED VARIABLES	*
  PL_is_attvar  := ProcAddress(Handle, 'PL_is_attvar');
  PL_get_attr  := ProcAddress(Handle, 'PL_get_attr');
  // *           TABLING            *
  //PL_get_delay_list  := ProcAddress(Handle, 'PL_get_delay_list');
  //*	      ERRORS		*
  PL_get_atom_ex  := ProcAddress(Handle, 'PL_get_atom_ex');
  PL_get_integer_ex  := ProcAddress(Handle, 'PL_get_integer_ex');
  PL_get_long_ex  := ProcAddress(Handle, 'PL_get_long_ex');
  PL_get_int64_ex  := ProcAddress(Handle, 'PL_get_int64_ex');
  PL_get_uint64_ex  := ProcAddress(Handle, 'PL_get_uint64_ex');
    //PL_get_intptr_ex : function(t : term_t; intptr_t *i);
  PL_get_intptr_ex  := ProcAddress(Handle, 'PL_get_intptr_ex');
  PL_get_size_ex  := ProcAddress(Handle, 'PL_get_size_ex');
  PL_get_bool_ex := ProcAddress(Handle, 'PL_get_bool_ex');
  PL_get_float_ex  := ProcAddress(Handle, 'PL_get_float_ex');
  PL_get_char_ex  := ProcAddress(Handle, 'PL_get_char_ex');
  PL_unify_bool_ex  := ProcAddress(Handle, 'PL_unify_bool_ex');
  PL_get_pointer_ex  := ProcAddress(Handle, 'PL_get_pointer_ex');
  PL_unify_list_ex  := ProcAddress(Handle, 'PL_unify_list_ex');
  PL_unify_nil_ex  := ProcAddress(Handle, 'PL_unify_nil_ex');
  PL_get_list_ex  := ProcAddress(Handle, 'PL_get_list_ex');
  PL_get_nil_ex  := ProcAddress(Handle, 'PL_get_nil_ex');
  PL_instantiation_error  := ProcAddress(Handle, 'PL_instantiation_error');
  PL_uninstantiation_error  := ProcAddress(Handle, 'PL_uninstantiation_error');
  PL_representation_error  := ProcAddress(Handle, 'PL_representation_error');
  PL_type_error  := ProcAddress(Handle, 'PL_type_error');
  PL_domain_error  := ProcAddress(Handle, 'PL_domain_error');
  PL_existence_error  := ProcAddress(Handle, 'PL_existence_error');
  PL_permission_error  := ProcAddress(Handle, 'PL_permission_error');
  PL_resource_error  := ProcAddress(Handle, 'PL_resource_error');
  // FILENAME SUPPORT
  PL_get_file_name := ProcAddress(Handle, 'PL_get_file_name');
  PL_changed_cwd  := ProcAddress(Handle, 'PL_changed_cwd');
  PL_cwd  := ProcAddress(Handle, 'PL_cwd');


  SP_set_state  := ProcAddress(Handle, 'SP_set_state');
  SP_get_state  := ProcAddress(Handle, 'SP_get_state');
  //*	     COMPARE		*
  PL_compare  := ProcAddress(Handle, 'PL_compare');
  PL_same_compound  := ProcAddress(Handle, 'PL_same_compound');
  // RECORDED DATABASE
  PL_record  := ProcAddress(Handle, 'PL_record');
  PL_recorded  := ProcAddress(Handle, 'PL_recorded');
  PL_erase  := ProcAddress(Handle, 'PL_erase');
  PL_duplicate_record  := ProcAddress(Handle, 'PL_duplicate_record');
  PL_record_external  := ProcAddress(Handle, 'PL_record_external');
  PL_recorded_external  := ProcAddress(Handle, 'PL_recorded_external');
  PL_erase_external  := ProcAddress(Handle, 'PL_erase_external');
  // INTERNAL FUNCTIONS
  _PL_get_atomic  := ProcAddress(Handle, '_PL_get_atomic');
  _PL_put_atomic  := ProcAddress(Handle, '_PL_put_atomic');
  _PL_unify_atomic  := ProcAddress(Handle, '_PL_unify_atomic');
  //_PL_copy_atomic  := ProcAddress(Handle, '_PL_copy_atomic');
  _PL_get_arg  := ProcAddress(Handle, '_PL_get_arg');
  PL_chars_to_term  := ProcAddress(Handle, 'PL_chars_to_term');
  PL_put_term_from_chars  := ProcAddress(Handle, 'PL_put_term_from_chars');
	 //*	    EMBEDDING		*
  PL_initialise  := ProcAddress(Handle, 'PL_initialise');
  PL_is_initialised  := ProcAddress(Handle, 'PL_is_initialised');
  PL_cleanup  := ProcAddress(Handle, 'PL_cleanup');
  PL_halt  := ProcAddress(Handle, 'PL_halt');
  PL_toplevel  := ProcAddress(Handle, 'PL_toplevel');
  // procedure PL_install_readline(); cdecl; external 'libswipl.dll';
  // PL_EXPORT(int)		PL_winitialise(int argc, wchar_t **argv);
  // PL_EXPORT(int)		PL_set_resource_db_mem(const unsigned char *data,
  //*	  DYNAMIC LINKING	*
  PL_dlopen  := ProcAddress(Handle, 'PL_dlopen');
  PL_dlerror  := ProcAddress(Handle, 'PL_dlerror');
  PL_dlsym  := ProcAddress(Handle, 'PL_dlsym');
  PL_dlclose  := ProcAddress(Handle, 'PL_dlclose');
  // *      INPUT/PROMPT/ETC	*
  PL_dispatch  := ProcAddress(Handle, 'PL_dispatch');
  PL_add_to_protocol  := ProcAddress(Handle, 'PL_add_to_protocol');
  PL_prompt_string  := ProcAddress(Handle, 'PL_prompt_string');
  PL_write_prompt  := ProcAddress(Handle, 'PL_write_prompt');
  PL_prompt_next  := ProcAddress(Handle, 'PL_prompt_next');
  PL_atom_generator  := ProcAddress(Handle, 'PL_atom_generator');
  // MEMORY ALLOCATION
  PL_malloc  := ProcAddress(Handle, 'PL_malloc');
  PL_malloc_atomic  := ProcAddress(Handle, 'PL_malloc_atomic');
  PL_malloc_uncollectable  := ProcAddress(Handle, 'PL_malloc_uncollectable');
  PL_malloc_unmanaged  := ProcAddress(Handle, 'PL_malloc_unmanaged');
  PL_malloc_atomic_unmanaged  := ProcAddress(Handle, 'PL_malloc_atomic_unmanaged');
  PL_realloc  := ProcAddress(Handle, 'PL_realloc');
  PL_free  := ProcAddress(Handle, 'PL_realloc');
  PL_linger  := ProcAddress(Handle, 'PL_linger');
  // *             HOOKS		*
  PL_dispatch_hook := ProcAddress(Handle, 'PL_dispatch_hook');
  PL_abort_hook  := ProcAddress(Handle, 'PL_abort_hook');
  PL_initialise_hook  := ProcAddress(Handle, 'PL_initialise_hook');
  PL_abort_unhook  := ProcAddress(Handle, 'PL_abort_unhook');
  //PL_async_hook  := ProcAddress(Handle, 'PL_async_hook');
  PL_agc_hook  := ProcAddress(Handle, 'PL_agc_hook');

  PL_query  := ProcAddress(Handle, 'PL_query');
  // PROLOG THREADS	*
  PL_thread_self  := ProcAddress(Handle, 'PL_thread_self');
  PL_unify_thread_id  := ProcAddress(Handle, 'PL_unify_thread_id');
  PL_get_thread_id_ex  := ProcAddress(Handle, 'PL_get_thread_id_ex');
  PL_get_thread_alias  := ProcAddress(Handle, 'PL_get_thread_alias');
  PL_thread_attach_engine  := ProcAddress(Handle, 'PL_thread_attach_engine');
  PL_thread_destroy_engine  := ProcAddress(Handle, 'PL_thread_destroy_engine');
  PL_thread_at_exit  := ProcAddress(Handle, 'PL_thread_at_exit');
{$IFDEF WINDOWS}
  PL_w32thread_raise  := ProcAddress(Handle, 'PL_w32thread_raise');
  PL_wait_for_console_input  := ProcAddress(Handle, 'PL_wait_for_console_input');
  PL_w32_wrap_ansi_console  := ProcAddress(Handle, 'PL_w32_wrap_ansi_console');
  PL_w32_running_under_wine  := ProcAddress(Handle, 'PL_w32_running_under_wine');
{$ENDIF}
  PL_create_engine  := ProcAddress(Handle, 'PL_create_engine');
  PL_set_engine  := ProcAddress(Handle, 'PL_set_engine');
  PL_destroy_engine  := ProcAddress(Handle, 'PL_destroy_engine');

  PL_mark_string_buffers              := ProcAddress(Handle, 'PL_mark_string_buffers');
  PL_release_string_buffers_from_mark := ProcAddress(Handle, 'PL_release_string_buffers_from_mark');

  // available if version >= 9


  PL_can_yield  := ProcAddress(Handle, 'PL_can_yield');
  // *    QUINTUS/SICSTUS WRAPPER	*
  PL_cvt_i_bool := ProcAddress(Handle, 'PL_cvt_i_bool');
  PL_cvt_i_char := ProcAddress(Handle, 'PL_cvt_i_char');
  PL_cvt_i_schar := ProcAddress(Handle, 'PL_cvt_i_schar');
  PL_cvt_i_uchar := ProcAddress(Handle, 'PL_cvt_i_uchar');
  PL_cvt_i_short := ProcAddress(Handle, 'PL_cvt_i_short');
  PL_cvt_i_ushort := ProcAddress(Handle, 'PL_cvt_i_ushort');
  PL_cvt_i_int  := ProcAddress(Handle, 'PL_cvt_i_int');
  PL_cvt_i_uint  := ProcAddress(Handle, 'PL_cvt_i_uint');
  PL_cvt_i_long  := ProcAddress(Handle, 'PL_cvt_i_long');
  PL_cvt_i_ulong  := ProcAddress(Handle, 'PL_cvt_i_ulong');
  PL_cvt_i_int32  := ProcAddress(Handle, 'PL_cvt_i_int32');
  PL_cvt_i_uint32  := ProcAddress(Handle, 'PL_cvt_i_uint32');
  PL_cvt_i_int64  := ProcAddress(Handle, 'PL_cvt_i_int64');
  PL_cvt_i_uint64  := ProcAddress(Handle, 'PL_cvt_i_uint64');
  PL_cvt_i_size_t  := ProcAddress(Handle, 'PL_cvt_i_size_t');
  PL_cvt_i_float  := ProcAddress(Handle, 'PL_cvt_i_float');
  PL_cvt_i_single  := ProcAddress(Handle, 'PL_cvt_i_single');
  PL_cvt_i_string  := ProcAddress(Handle, 'PL_cvt_i_string');
  PL_cvt_i_codes  := ProcAddress(Handle, 'PL_cvt_i_codes');
  PL_cvt_i_atom  := ProcAddress(Handle, 'PL_cvt_i_atom');
  PL_cvt_i_address  := ProcAddress(Handle, 'PL_cvt_i_address');
  PL_cvt_o_int64  := ProcAddress(Handle, 'PL_cvt_o_int64');
  PL_cvt_o_float  := ProcAddress(Handle, 'PL_cvt_o_float');
  PL_cvt_o_single  := ProcAddress(Handle, 'PL_cvt_o_single');
  PL_cvt_o_string  := ProcAddress(Handle, 'PL_cvt_o_string');
  PL_cvt_o_codes  := ProcAddress(Handle, 'PL_cvt_o_codes');
  PL_cvt_o_atom  := ProcAddress(Handle, 'PL_cvt_o_atom');
  PL_cvt_o_address  := ProcAddress(Handle, 'PL_cvt_o_address');
  PL_new_nil_ref  := ProcAddress(Handle, 'PL_new_nil_ref');
  /// The default is UTF-8 (REP_UTF8)
  PL_cvt_encoding  := ProcAddress(Handle, 'PL_cvt_encoding');
  PL_cvt_set_encoding  := ProcAddress(Handle, 'PL_cvt_set_encoding');

  PL_version_info  := ProcAddress(Handle, 'PL_version_info');

  (*
  https://www.swi-prolog.org/pldoc/man?section=embedded
  https://eu.swi-prolog.org/pldoc/man?section=foreign
  embed in prolog
  https://eu.swi-prolog.org/pldoc/man?section=foreigninclude
  embed prolog in delphi
  https://eu.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/pengines.html%27%29
  https://stackoverflow.com/questions/65118493/is-there-any-reasonable-way-to-embed-a-prolog-interpreter-inside-of-a-c-program
  *)
end;



end.
