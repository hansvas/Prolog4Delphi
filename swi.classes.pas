Unit swi.classes;
Interface
Uses classes,
     System.Character,
     System.Generics.Collections,
     swi.prolog;

Type TProlog = Class;

     TPrologArgumentType = (
      variable  ,    // PL_put_variable
      atom      ,    // PL_put_atom
      atom_chars,    // PL_put_atom_chars

      string_chars,  // PL_put_string_chars
      list_chars  ,  // PL_put_list_chars,
      list_codes  ,  // PL_put_list_codes

      atom_nchars  , // PL_put_atom_nchars
      string_nchars, // PL_put_string_nchars
      list_nchars,   // PL_put_list_nchars
      list_ncodes,   // PL_put_list_ncodes
      nt,           // PL_put_integer
      pnt,           // PL_put_pointer
      float,         // PL_put_float
      functor,       // PL_put_functor
      lst,           // PL_put_list
      // PL_put_nil
      term,          // PL_put_term
      bl,            // PL_put_bool
      chrs,          // PL_put_chars
      dict,          // pl_put_dict
      nt64,          // PL_put_int64
      unt64,         // PL_put_uint64
      term_from_chars // PL_put_term_from_chars
    );

    TPrologArgumentCall  = (
     callIn,
     callOut,
     callInOut );

    rArgument = record
     tpe  : TPrologArgumentType;
     call : TPrologArgumentCall;
     Constructor Create
       (aType : TPrologArgumentType; aCall : TPrologArgumentCall);
    end;

    TPrologArgumentsList = Array Of rArgument;

     rPredicate = Record
       arity       : Integer;
       module      : AnsiString;
       callable    : Boolean;
       predicate   : predicate_t;
       arguments   : TPrologArgumentsList;
       description : String;
     End;

     TPredicates = TDictionary<AnsiString,rPredicate>;

     TPredicatedFunc = reference To
       Function(p : TProlog; pred : rPredicate; Var trm : term_t) : Boolean;

     ///<summary>
     /// The Prolog engine can be called from Delphi. There are two interfaces
     /// for this. For the first, a term is created that could be used as an
     /// argument to call/1, and then PL_call() is used to call Prolog. This
     /// system is simple, but does not allow to inspect the different answers
     /// to a non-deterministic goal and is relatively slow as the runtime system
     /// needs to find the predicate. The other interface is based on
     /// needs to find the predicate. The other interface is based on
     /// PL_open_query(), PL_next_solution(), and PL_cut_query() or
     /// PL_close_query(). This mechanism is more powerful, but also more
     /// complicated to use.
     ///
     /// Finding all Solutions to a Goal :
     ///    https://www.swi-prolog.org/pldoc/man?section=allsolutions
     ///
     /// This class helps with the registration of the predicates and also
     /// the normally used arguments.
     ///
     /// </summary>


     TProlog = Class(TComponent)
     private
      rFindAll    : rPredicate;
      FPredicates : TPredicates;
      // the environment used during a call to initialize must
      // live for the complete runtime of a prolog engine
      FEnvironment : Array Of PAnsiChar;
      FEnvLen      : Integer;
      FSWIPath     : String;

      FParamStart  : Word;
      FUseParams   : Boolean;
      FInitialized : Boolean;
      FAutoLoad    : Boolean;

      FUserModule  : module_t;
     protected

      Function makeTermOfAtoms
       (params : array of const; references : TStrings): term_t; virtual;
      Function makeTerm (p : rPredicate; params : Array Of Const): term_t; virtual;
      ///<summary>Override this method if your reinitialising process is
      ///  different from the normal initialising process.</summary>
      Function ReInitialize : Boolean;              virtual;

      ///<summary>Calls pl_Halt or pl_Cleanup, depending from the parameter
      ///  useHalt. Did not return if useHalt = True, so if you want to
      ///  call a prolog subroutine and use delphi afterwards, useHalt
      ///  must be false</summary>
      Function HaltOrCleanUp(useHalt : Boolean = False) : Boolean; virtual;

      Procedure SetDLLPath(Const Value: String);    virtual;
      ///<summary>Allows you to transfer the command line paramters to the
      ///  prolog kernal during initialization</summary>
      Procedure SetUseParams(Const Value: Boolean); virtual;
      ///<summary>DoInitialise can be called with or withour UseParams set.
      /// If you use aParams they will be added to the environment
      /// afterwards</summary>
      Procedure DoInitialise(aParams : TStrings); virtual;
      ///<summary>clears all predicates, also the default ones. If you want
      /// the default predicates registered, call registerDefaultPredicates
      /// after this method</summary>
      Procedure ClearPredicates; virtual;
      ///<summary>Registers the default predicates like 'consult'</summary>
      Procedure RegisterDefaultPredicates; virtual;

     public
      Constructor Create(aOwner : TComponent);      override;
      Destructor Destroy; override;

      Function Initialize    (additional : TStrings = Nil) : Boolean; virtual;
      ///<summary>Calls PL_Toplevel and cleans up. The result is
      ///  the result from PL_toplevel</summary>
      Function RunAndCleanToplevel : Boolean; virtual;

      Function RegisterPredicate(
       Const aName, module : AnsiString; callable : Boolean;
       Const Description : String = '';
       Const argumentList : TPrologArgumentsList = []) : rPredicate; virtual;

      Function getPredicate(Const aName : AnsiString) : rPredicate; virtual;

      Function ConsultFile(Const aFileName : AnsiString ) : Boolean; virtual;

      // do not use, warning ! not working
      ///<summary>Allows you to query a predicate. For each iteration
      ///  runFunc will be called !after! a call to pl_next_solution.
      ///  if runFunc fails, the complete execution will stop and the
      ///  method queryPredicate will also fail.</summary>
      Function QueryPredicate(Const aName : AnsiString;
        Params : Array Of Const;
        runFunc   : TPredicatedFunc;
        cleanFunc : TPredicatedFunc = Nil) : Boolean; overload; virtual;

       // do not use, warning ! not working
      Function QueryPredicate(Const aName : AnsiString;
        makeParamsFunc : TPredicatedFunc;
        runFunc   : TPredicatedFunc;
        cleanFunc : TPredicatedFunc = Nil) : Boolean; overload; virtual;

      ///<summary>Use this for calling (not querying) a predicate</summary>
      Function CallPredicate(Const aName : AnsiString;
        makeParamsFunc : TPredicatedFunc;
        cleanFunc      : TPredicatedFunc = Nil ) : Boolean; overload; virtual;

      ///<summary>Use this for calling (not querying) a predicate</summary>
      Function CallPredicate(Const aName : AnsiString;
        Params : Array Of Const;
        cleanFunc      : TPredicatedFunc = Nil ) : Boolean; overload; virtual;


      ///<summary> Uses findall to call a predicate, the template is
      ///  build only from variables like
      ///
      ///  findall(X,predicate(...), Lst)
      ///
      /// </summary>

      {Function CallFindAll(Const aName : AnsiString;
        template  : Array of Const; // Variable - String -> Variable
        Params    : Array Of Const;
        cleanFunc : TPredicatedFunc = Nil ) : Boolean; virtual; }


      // -----------------------------------------------------------------------
      function ConstructFunctorFromPredicate
       (const aName : AnsiString;
        params : array of const ) : functor_t; virtual;
      Function Term2StringList(lst : term_t) : TStrings; virtual;
     Property
      AutoLoad : Boolean
       read FAutoLoad write FAutoLoad;
     Property
      Initialized : Boolean
       read FInitialized;
     Property
      UseParams : Boolean
       read FUseParams write SetUseParams;
     Property
       SWIPath : String
        read FSWIPath write SetDLLPath;
     End;

Implementation
Uses Windows,
     SysUtils;

Resourcestring

  E_NOT_IMPLEMENTED   = 'Not implemented';
  E_NO_PREDICATE      = 'Predicate %s not registered';
  E_ERROR_DURING_CALL = 'Error during execution of %s';



Constructor rArgument.Create
       (aType : TPrologArgumentType; aCall : TPrologArgumentCall);
begin
  tpe  := aType;
  call := aCall;
end;

{ TODO : fill missing types }

Function VarRecToStrA( AVarRec : TVarRec ) : AnsiString;
Const Bool : Array[Boolean] Of String = ('False', 'True');
  Begin
    Case AVarRec.VType Of
      vtInteger:    Result := IntToStr(AVarRec.VInteger);
      vtBoolean:    Result := Bool[AVarRec.VBoolean];
      vtChar:       Result := AVarRec.VChar;
      vtExtended:   Result := FloatToStr(AVarRec.VExtended^);
      vtString:     Result := AVarRec.VString^;
      vtPChar:      Result := AVarRec.VPChar;
      vtObject:     Result := AVarRec.VObject.ClassName;
      vtClass:      Result := AVarRec.VClass.ClassName;
      vtAnsiString: Result := AnsiString(AVarRec.VAnsiString);
      vtCurrency:   Result := CurrToStr(AVarRec.VCurrency^);
      vtVariant:    Result := String(AVarRec.VVariant^);
    Else
      result := '';
    End;
  End;

{ TProlog }

Constructor TProlog.Create(aOwner : TComponent);
Begin
  Inherited Create(aOwner);
  FAutoLoad := True;
  FUseParams := False;
  FInitialized := False;
  FUserModule := Nil;
  // 0 wenn der Programmname mit übergeben werden soll
  FParamStart := 1;
  FPredicates := TPredicates.Create;
End;

Destructor TProlog.Destroy;
Begin
  If Initialized Then
     HaltOrCleanUp(False);
  SetLength(FEnvironment,0);
  ClearPredicates;
  FPredicates.Free;
  Inherited Destroy;
End;

Procedure TProlog.RegisterDefaultPredicates;
Begin
  RegisterPredicate('consult','',True,
  'Consult is used to load a prolog source file or a library',
  [rArgument.Create(atom_chars,callIn)]);

  rFindAll := RegisterPredicate('findall','',True,
  'In its simpliest form, the query findall produces a list of all the objects ' +
  'that satisfy the goal Goal. Often Object is simply a variable, in which case ' +
  'the query can be read as: Give me a list containing all the instantiations ' +
  'of Object which satisfy Goal.',
   [rArgument.Create(term, callIn),
    rArgument.Create(term, callIn),
    rArgument.Create(lst , callOut)]);
End;

Procedure TProlog.ClearPredicates;
Begin
  FPredicates.Clear;
End;

Function TProlog.HaltOrCleanUp(useHalt : Boolean) : Boolean;
Begin
 If Initialized Then
  Begin
    If useHalt Then
     result := CHCK(PL_Halt(1))
    Else
     Case PL_cleanup(1) Of
       PL_CLEANUP_CANCELED  : result := False;
       PL_CLEANUP_SUCCESS   : result := True;
       PL_CLEANUP_FAILED    : result := False;
       PL_CLEANUP_RECURSIVE : result := True;
       Else result := True;
     End;
    FInitialized := False;
  End Else
  result := False;
End;

Function TProlog.Initialize
        (additional : TStrings = Nil) : Boolean;
Var res : Integer;
    err : String;
Begin
 result := False;
 If FInitialized Then
    exit; //->

 If (AutoLoad) And Not (swi.prolog.Loaded) Then
  Begin
    If FileExists(SWIPath) Then
       LoadSWIProlog(SWIPath);
  End;

 If useParams Then
    FEnvLen := (ParamCount-FParamStart) + 1
 Else
    FEnvLen := 1;

 If Assigned(additional) Then
    FEnvLen := FEnvLen + additional.Count;

 SetLength(FEnvironment, FEnvLen);
 For Var i := 0 To FEnvLen-1 Do
     FEnvironment[i] := Nil;

 If UseParams Then
  Begin
    For Var i := FParamStart To ParamCount Do
        FEnvironment[i] := PAnsiChar(ParamStr(i));
  End ;
  DoInitialise(Additional);
 Try
   res := PL_initialise(FEnvLen, @FEnvironment);
   result := chck(res);
 Except
   res := GetLastError();
   err := SysErrorMessage(res);
   Raise Exception.Create(err)
 End;

 If result Then
    RegisterDefaultPredicates;

 FInitialized := Result;
 If FInitialized Then
  Begin
    FUserModule := pl_context;
  End;

End;

Procedure TProlog.DoInitialise(aParams : TStrings);
Var start : Integer;
Begin
 If Assigned(aParams) Then
 Begin
  If useParams Then
    start := ParamCount
  Else
    start := 0;
  For Var i := 0 To aParams.Count-1 Do
      FEnvironment[start+i] := PAnsiChar(aParams[i]);
 End;
End;

Function TProlog.ReInitialize : Boolean;
Begin
  result := Initialize;
End;

Procedure TProlog.SetDLLPath(Const Value: String);
Begin
 If Value <> FSWIPath Then
  Begin
    FSWIPath := Value;
    If FInitialized Then
    Begin
       If swi.prolog.Loaded Then
          swi.prolog.Reset;
       FInitialized := ReInitialize;
    End;
  End;
End;

Procedure TProlog.SetUseParams(Const Value: Boolean);
Begin
 If Value <> FUseParams Then
 Begin
  FUseParams := Value;
  If FInitialized Then
       FInitialized := ReInitialize;
 End;
End;

Function TProlog.RunAndCleanToplevel : Boolean;
Begin
 result := False;
 If Initialized Then
  Begin
   result := CHCK(PL_Toplevel);
   HaltOrCleanUp(False);
   FInitialized := False;
  End ;
End;

////////////////////////////////////////////////////////////////////////////////
///

Function TProlog.RegisterPredicate(
       Const aName, module : AnsiString; callable : Boolean;
       Const Description : String = '';
       Const argumentList : TPrologArgumentsList = []) : rPredicate;
Var  r : rPredicate;
Begin
 r.arity    := Length(argumentList);
 r.callable := callable;
 r.module   := module;
 r.predicate := PL_Predicate( PAnsiChar(aName), r.arity, PAnsiChar(Module));
 r.arguments := argumentList;
 r.description := Description;
 Try
  FPredicates.AddOrSetValue(aName,r);
 Except
  Raise;
 End;
 result := r;
End;

Function TProlog.makeTermOfAtoms
   (params : array of const; references : TStrings): term_t;
Var a : TVarRec;
  len : Integer;
  int : Integer;
  ans : AnsiString;
  str : String;
Begin
 len := Length(params);
 result := pl_new_term_refs(len);
 For Var i := 0 To len-1 Do
  Begin
   a   := Params[i];
   Case a.VType Of
     vtInteger    : Begin
      pl_put_atom(result + i, atom_t(a.VInteger));
     End;
     vtInt64      : Begin
               pl_put_atom(result + i, a.VInt64^);
     End;
     vtVariant    : Begin
      int := a.VVariant^;
      pl_put_atom(result + i, int);
     End;
     vtString     : Begin
       ans := a.VString^;
       pl_put_atom_nchars(result + i, length(ans), pAnsiChar(ans));
       if length(ans) = 1 then
          references.Add(ans)
     end;
     vtAnsiString : Begin
       ans := AnsiString(a.VAnsiString);
       pl_put_atom_nchars(result + i, length(ans), pAnsiChar(ans));
       if length(ans) = 1 then
          references.Add(ans)
     End;

     // vtwidechar
   End
  End;
  // https://stackoverflow.com/questions/55955656/how-to-get-the-result-from-prolog-via-the-c-ffi-into-a-c-variable
end;

Function TProlog.makeTerm (p : rPredicate; params : Array Of Const): term_t;
Var a : TVarRec;
  int : Integer;
  ans : AnsiString;
  str : String;
  arg : rArgument;
Begin
   If p.arity > 0 Then
   Begin
      result := pl_new_term_refs(p.arity);
      For Var i := 0 To p.arity-1 Do
      Begin
        a   := Params[i];
        arg := p.arguments[i];
        if arg.call in [callIn,callInOut] then
        Case arg.tpe Of
          variable : begin
            pl_put_variable(result + i);
          end;
          atom :  Begin    // PL_put_atom
            Case a.VType Of
             vtInteger    : Begin
               pl_put_atom(result + i, atom_t(a.VInteger));
             End;
             vtInt64      : Begin
               pl_put_atom(result + i, a.VInt64^);
             End;
             vtVariant    : Begin
               int := a.VVariant^;
               pl_put_atom(result + i, int);
             End;
             vtString     : Begin
               str := a.VString^;
               if (not TryStrToInt(str,int)) and
                  (str<>'') then
                begin
                 if length(str) = 1 then
                   begin
                     ans := str;
                     pl_put_atom_nchars(result + i, 1, pAnsiChar(ans));
                   end else
                   begin
                     str := uppercase(str);
                    if str = 'VARIABLE' then
                        pl_put_variable(result + i);
                   end;
                end
               else
                pl_put_atom(result + i, int);
             End;
             vtAnsiString : Begin
               ans := AnsiString(a.VAnsiString);
               if not TryStrToInt(ans,int) then
                begin
                 if length(str) = 1 then
                   begin
                       pl_put_atom_nchars(result + i, 1, pAnsiChar(ans));
                   end else
                   begin
                     ans := uppercase(ans);
                     if ans = 'VARIABLE' then
                       pl_put_variable(result + i);
                   end;
                end else
               pl_put_atom(result + i, int);
             End;
            vtWideChar : begin
               str := a.VWideChar;
               if (not TryStrToInt(str,int)) and
                  (str<>'') then
                begin
                 if length(str) = 1 then
                   begin
                     ans := str;
                     pl_put_atom_nchars(result + i, 1, pAnsiChar(ans));
                   end else
                   begin
                     str := uppercase(str);
                    if str = 'VARIABLE' then
                        pl_put_variable(result + i);
                   end;
                end
               else
                pl_put_atom(result + i, int);
            end;
           end;
          End;
      atom_chars : Begin    // PL_put_atom_chars
       ans := VarRecToStrA(a);
       pl_put_atom_chars(result+i, PAnsiChar(ans));
      End;

      string_chars : Begin  // PL_put_string_chars
       ans := VarRecToStrA(a);
       pl_put_string_chars(result+i, PAnsiChar(ans));
      End;
      list_chars  ,  // PL_put_list_chars,
      list_codes  :  // PL_put_list_codes
      Begin
        Raise Exception.Create(E_NOT_IMPLEMENTED);
      End;

      atom_nchars  : Begin // PL_put_atom_nchars
       ans := VarRecToStrA(a);
       pl_put_atom_nchars(result+i, Length(ans), PAnsiChar(ans));
      End;
      string_nchars : Begin //  // PL_put_string_nchars
       ans := VarRecToStrA(a);
       pl_put_string_nchars(result+i, Length(ans), PAnsiChar(ans));
      End;
      list_nchars,   // PL_put_list_nchars
      list_ncodes : Begin   // PL_put_list_ncodes
          Raise Exception.Create(E_NOT_IMPLEMENTED);
      End;

      { TODO : fill missing types }

      (*nt : begin           // PL_put_integer
       PL_put_integer(trm+i,  i: LongInt);
      end;
      pnt,           // PL_put_pointer
      float,         // PL_put_float
      functor,       // PL_put_functor
      lst,           // PL_put_list
      // PL_put_nil
      term,          // PL_put_term
      bl,            // PL_put_bool
      chrs,          // PL_put_chars
      dict,          // pl_put_dict

      nt64,          // PL_put_int64
      unt64,         // PL_put_uint64
      term_from_chars // PL_put_term_from_chars *)
      End;
     End;
   End Else
   result := 0;
End;

Function TProlog.getPredicate(Const aName : AnsiString) : rPredicate;
Begin
  If Not FPredicates.TryGetValue(aName,result) Then
     Raise Exception.Create(Format(E_NO_PREDICATE,[aName]));
End;


Function TProlog.ConsultFile(Const aFileName : AnsiString ) : Boolean;
Begin
  result := FileExists(aFileName);
  If result Then
     result := CallPredicate('consult', [aFileName]);
End;

Function TProlog.CallPredicate(Const aName : AnsiString;
        makeParamsFunc : TPredicatedFunc;
        cleanFunc      : TPredicatedFunc = Nil) : Boolean;
Var p : rPredicate;
  trm : term_t;
Begin
 result := false;
 If FPredicates.TryGetValue(aName, p) Then
  Begin
   If p.arity > 0 Then
   Begin
      trm := pl_new_term_refs(p.arity);
      result := makeParamsFunc(self,p,trm);
   End Else
   trm := 0;

   If result Then
    Begin
     //pred := PL_Predicate( PAnsiChar(aName), p.arity, PAnsiChar(p.module));
     //result := CHCK(PL_call_predicate(NIL, PL_Q_NORMAL, pred, trm));
     result := CHCK(PL_call_predicate(Nil, PL_Q_NORMAL, p.predicate, trm));
     If Assigned(cleanFunc) Then
      result := cleanFunc(self,p,trm) And result;
    End;

  End;
End;

Function TProlog.CallPredicate(Const aName : AnsiString;
        Params : Array Of Const;
        cleanFunc      : TPredicatedFunc = Nil ) : Boolean;
Var p : rPredicate;
  trm : term_t;
Begin
 result := True;
 If FPredicates.TryGetValue(aName, p) Then
  Begin
     trm := makeTerm (p, params);
     result := CHCK(PL_call_predicate(Nil, PL_Q_NORMAL, p.predicate, trm));
     If Assigned(cleanFunc) Then
        result := cleanFunc(self,p,trm) And result;
  End;
End;


// http://ilab.usc.edu/sdoc/html/d7/da8/SWIProlog_8C_source.html
Function TProlog.QueryPredicate(Const aName : AnsiString;
        makeParamsFunc : TPredicatedFunc;
        runFunc        : TPredicatedFunc;
        cleanFunc      : TPredicatedFunc = Nil ) : Boolean;
Var  p : rPredicate;
   trm : term_t;
   exc : term_t;
     q : qid_t;
Begin
 result := True;
 If FPredicates.TryGetValue(aName, p) Then
  Begin
   If p.arity > 0 Then
   Begin
      trm := pl_new_term_refs(p.arity);
      result := makeParamsFunc(self,p,trm);
   End Else
   trm := 0;
   q := PL_open_query(FUserModule,
    PL_Q_NORMAL Or PL_Q_CATCH_EXCEPTION, p.predicate, trm);
   While result And ( CHCK(pl_next_solution(q)) ) Do
    Begin
      exc := PL_Exception(q);
      { TODO : auswerten }
      result := runFunc(self,p, trm);
    End;
   pl_close_query(q);
   If Assigned(cleanFunc) Then
      result := cleanFunc(self,p,trm) And result;
  End;
End;

(*https://cpp.hotexamples.com/examples/-/-/PL_open_query/cpp-pl_open_query-function-examples.html*)

Function TProlog.QueryPredicate(Const aName : AnsiString;
        Params : Array Of Const;
        runFunc   : TPredicatedFunc;
        cleanFunc : TPredicatedFunc = Nil) : Boolean;
Var p : rPredicate;
  trm : term_t;
    q : qid_t;
Begin
 result := True;
 If FPredicates.TryGetValue(aName, p) Then
  Begin
   trm := makeTerm(p,params);
   q := PL_open_query(Nil, PL_Q_NORMAL, p.predicate, trm);
   Try
    While result And ( CHCK(pl_next_solution(q)) ) Do
         result := runFunc(self,p, trm);
   Finally
     pl_close_query(q);
   End;
   If Assigned(cleanFunc) Then
      result := cleanFunc(self,p,trm) And result;
  End;
End;

function TProlog.ConstructFunctorFromPredicate
       (const aName : AnsiString; params : array of const) : functor_t;
var    p : rPredicate;
   terms : term_t;
   funct : functor_t;
begin
  if FPredicates.TryGetValue(aName, p) then
   begin
    funct  := PL_new_functor(PL_new_atom(PAnsiChar(aName)), p.arity);
    result := PL_new_term_ref();
    // if Params = 0 then all params are variables
    if Length(params) = 0 then
     begin
      terms := PL_new_term_refs(p.arity);
      for var i := 0 to p.arity-1 do
        pl_put_variable(terms + i);
     end else
     begin
       terms := makeTerm(p, params);
       pl_cons_functor_v(result, funct , terms);
     end;
   end else
   raise Exception.Create(Format(E_NO_PREDICATE, [aName]));
end;

Function TProlog.Term2StringList(lst : term_t) : TStrings;
Var head, tail : term_t;
    rc  : Integer;
    s : PAnsiChar;

    mark : buf_mark_t;
Begin
 head := pl_new_term_ref();
 tail := pl_copy_term_ref(lst);

 result := TStringList.Create;
 rc := 1;
 While (chck(rc) And chck(PL_get_list_ex(tail, head, tail))) Do
  Begin
    PL_STRINGS_MARK(mark);
    Try
      rc := PL_get_chars(head, s, CVT_ATOM Or REP_MB Or CVT_EXCEPTION);
      result.Add(s);
    Finally
      PL_STRINGS_RELEASE(mark);
    End;
  End;
End;



End.
