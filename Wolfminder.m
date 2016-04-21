
BeginPackage["Wolfminder`"]

usr::usage = "Beeminder username";

key::usage = "\
Beeminder personal auth key -- https://www.beeminder.com/api/v1/auth_token.json\
";

userget;
goalget;
goalput;
allgoals;
showroad;
(* dataput *)
(* dataget *)

retrat;

Begin["`Private`"];  (*********************************************************)

baseurl = "https://www.beeminder.com/api/v1/";

(***************************** Utility functions ******************************)
ARGV = args = Drop[$CommandLine, 4];        (* Command line args.             *)
pr = WriteString["stdout", ##]&;            (* More                           *)
prn = pr[##, "\n"]&;                        (*  convenient                    *)
perr = WriteString["stderr", ##]&;          (*   print                        *)
perrn = perr[##, "\n"]&;                    (*    statements.                 *)
re = RegularExpression;                     (* I wish Wolframaic weren't      *)
EOF = EndOfFile;                            (*   so damn verbose!             *)
read[] := InputString[""];                  (* Grab a line from stdin.        *)
readList[] := Most@                         (* Grab the list of all the lines *)
  NestWhileList[read[]&, read[], #=!=EOF&]; (*  from stdin.                   *)
cat = StringJoin@@(ToString/@{##})&;        (* Like sprintf/strout in C/C++.  *)
eval = ToExpression[cat[##]]&;              (* Like eval in every other lang. *)
slurp = Import[#, "Text"]&;                 (* Fetch contents of file as str. *)
spew[f_, stuf___] := With[{s= OpenWrite@f}, (* Write stuff to a file like Put *)
  WriteString[s, stuf]; Close[s]]           (*  but w/o quotes on strings.    *)
system = Run@cat@##&;                       (* System call.                   *)
backtick = Import[cat["!", ##], "Text"]&;   (* System call; returns stdout.   *)
                                            (* ABOVE: mma-scripting related.  *)
keys[f_, i_:1] :=                           (* BELOW: general utilities.      *)
  DownValues[f, Sort->False][[All,1,1,i]];  (* Keys of a hash/dictionary.     *)
SetAttributes[each, HoldAll];               (* each[pattern, list, body]      *)
each[pat_, lst_List, bod_] :=               (*  converts pattern to body for  *)
  (Cases[Unevaluated@lst, pat:>bod]; Null); (*   each element of list.        *)
each[p_, l_, b_] := (Cases[l, p:>b]; Null); (*    (Warning: eats Return[]s.)  *)
any[f_, l_List] := True ===                 (* Whether f applied to any       *)
  Scan[If[f[#], Return[True]]&, l];         (*  element of list is True.      *)
all[f_, l_List] := Null ===                 (* Similarly, And @@ f/@l         *)
  Scan[If[!f[#], Return[False]]&, l];       (*  (but with lazy evaluation).   *)
(******************************************************************************)

(* The builtin JSON parsing with ImportString fails on unicode strings.
   This seems to be more robust. 
   Cf http://stackoverflow.com/questions/2633003/parsing-and-generating-json *)
parseJSON[json_String] := With[{tr = {"["     -> "(*_MAGIC_TOKEN_[*){",
                                      "]"     -> "(*_MAGIC_TOKEN_]*)}",
                                      ":"     -> "(*_MAGIC_TOKEN_:*)->",
                                      "true"  -> "(*_MAGIC_TOKEN_t*)True",
                                      "false" -> "(*_MAGIC_TOKEN_f*)False",
                                      "null"  -> "(*_MAGIC_TOKEN_n*)Null",
                                      "e"     -> "(*_MAGIC_TOKEN_e*)*10^",
                                      "E"     -> "(*_MAGIC_TOKEN_E*)*10^"}},
  eval@StringReplace[cat@FullForm@eval[StringReplace[json, tr]], Reverse/@tr]]

(* Similar problems with the builtin ExportString[_, "JSON"] *)
jnum[x_] := StringReplace[
  ToString@NumberForm[N@x, ExponentFunction->(Null&)], re@"\\.$"->""]
genJSON[a_ -> b_]  := genJSON[a] <> ":" <> genJSON[b]
genJSON[{x__Rule}] := "{" <> cat @@ Riffle[genJSON /@ {x}, ", "] <> "}"
genJSON[{x___}]    := "[" <> cat @@ Riffle[genJSON /@ {x}, ", "] <> "]"
genJSON[Null]      := "null"
genJSON[True]      := "true"
genJSON[False]     := "false"
genJSON[x_]        := jnum[x] /; NumberQ[x]
genJSON[x_]        := "\"" <> StringReplace[cat[x], "\""->"\\\""] <> "\""


(* Mathematica 10.1 introduced UnixTime; this is a backport *)
(* And, annoyingly, Names["UnixTime"] is not the empty list in version 10.0 
   so we have to check the version number explicitly. *)
(*
If[$VersionNumber < 10.1,
  UPOCH = AbsoluteTime[DateObject[{1970,1,1, 0,0,0}, TimeZone->0]];
  UnixTime[x___] := Round[AbsoluteTime[x] - UPOCH];
  FromUnixTime[t_] := DateObject[AbsoluteTime[t+UPOCH]];
];
*)

DIY = 365.25; (* this is what physicists use, eg, to define a light year *)
SID = 86400;  (* seconds in day *)
SECS = <| "y" -> DIY*SID,
          "m" -> DIY/12*SID,
          "w" -> 7*SID,
          "d" -> SID,
          "h" -> 3600 |>;

(* Show Number. Convert x to string w/ no trailing dot. Target a total of sf 
   significant figures, clipped to be at least i and at most i+Ceiling[sf/2],
   where i is the number of digits in the integer part of x. Can also specify 
   explicit prefix strings for positive and negative numbers, typically for when
   you want an explicit plus sign in front positive numbers, like for specifying
   a delta. *)
Off[NumberForm::"sigz"]; (* o.w. ~10^100 in non-scientific notation complains *)
shn[x_, sf_:10, s_:{"-",""}] := If[!NumericQ[x], cat[x],
  With[{i= IntegerLength@IntegerPart@x, d = Ceiling[sf/2]},
    StringReplace[ToString@NumberForm[N@x, Clip[sf, {i,i+d}],
                                     ExponentFunction->(Null&), NumberSigns->s],
                  re@"\\.$"->""]]]
shns[x_, sf_:10] := shn[x, sf, {"-","+"}]

userget::usage = "Return Associaion of user info"
userget[] := Association@parseJSON@URLFetch[baseurl<>"users/me.json", 
  "Method"->"GET", "Parameters"->{"auth_token"->key}]

goalget::usage = "Return a hash of goal parameters for goal slug g";
goalget[g_] := Association@parseJSON@URLFetch[
  baseurl<>"users/me/goals/"<>g<>".json", "Method"->"GET",
    "Parameters"->{"auth_token"->key}]

(* Update goal with slug g with hash h *)
goalput[g_, h___] := URLFetch[baseurl<>"users/me/goals/"<>g<>".json", 
  "Method"->"PUT", "Parameters"->{"auth_token"->key, 
  Sequence@@(#1->genJSON[#2]& @@@ {h})}]

allgoals::usage = "Return a list of goal Associations"
allgoals[] := Association /@ parseJSON@URLFetch[baseurl<>"users/me/goals.json",
  "Method"->"GET", "Parameters"->{"auth_token"->key}]

shdt[Null] := ""
shdt[t_] := DateString[FromUnixTime[t], {"MonthNameShort", "Day"}]

shv[v_]      := If[v === Null, "", shn@v]
shr[r_, ru_] := If[r === Null, "", cat[shn@r, "/", ru]]

(* Helper for showroad that takes the road matrix and runits *)
showroad0[road_, ru_] := MatrixForm[{shdt@#1, shv@#2, shr[#3, ru]}& @@@ road]

(* Return human-friendly representation of a goal's YBR *)
showroad[gs_String] := With[{g = goalget[gs]},
  Grid[{{showroad0[g@"roadall", g@"runits"], Import[g@"thumb_url"]}}]]


(*
History-Destroying Retroratchet aka RETROratchet as opposed to Ratchet

Let b be the number of days of safety buffer you want.
MOAR/PHAT: tini,vini -> tcur+b-1,vcur
WEEN/RASH: tini,vini -> tcur,vcur+yaw*rcur*(b-1)
*)

(* Take start and end points of a road, tcur/vcur, runits, dir, yaw, and the 
   desired number of days of safety buffer (b) and return a new roadall that 
   goes from {tini,vini} to tfin -- no rows except the {tfin,vfin,rfin} row. *)
(* SCHDEL
rr0[{{tini_, vini_, _}, ___, {tfin_, vfin_, rfin_}}, tcur_, vcur_, runits_, 
    dir_, yaw_, b_] := With[{ru = SECS[runits]},
  If[dir*yaw > 0,
    {{tini,vini, Null}, {tfin, Null, (vcur-vini)/(tcur+b*SID-1-tini)*ru}},
    {{tini,vini, Null}, {tfin, Null, (vcur-vini)/(tcur-tini+yaw(b-1)*SID)*ru}}]]
*)
rr0[tini_, vini_, {tfin_, vfin_, _}, tcur_, vcur_, runits_, dir_, yaw_, b_] :=
  With[{u = SECS[runits]},
    If[dir*yaw > 0,
      {{tini,vini, Null}, {tfin, Null, (vcur-vini)/(tcur+b*SID-1-tini)*u}},
      {{tini,vini, Null}, {tfin, Null, (vcur-vini)/(tcur-tini+yaw(b-1)*SID)*u}}
  ]]

(* Take a goal slug g and retroratchet it so it has b days of safety buffer *)
retrat[gs_, b_] := Module[
  {g, siru, ra, dir, yaw, tini,vini, tcur,vcur, tfin,vfin,rfin, rr}, 
  g = goalget[gs];
  siru = SECS[g@"runits"];
  ra = g@"roadall";
  {dir, yaw} = {g@"dir", g@"yaw"};
  {tini, vini} = {g@"initday", g@"initval"};
  {tcur, vcur} = {g@"curday", g@"curval"};
  {tfin, vfin, rfin} = g@"mathishard";

  rr = {{tini, vini, Null}}; (* just tini/vini for first row *)
  AppendTo[rr, {tcur, Null, 
    If[dir*yaw > 0,
      (vcur-vini)/(tcur+b*SID-1-tini)*siru,
      (vcur-vini)/(tcur-tini+yaw*(b-1)*SID)*siru
    ]}];
  AppendTo[rr, {tfin, vfin}]

  rr = rr0[g@"initday", g@"initval", g@"mathishard", g@"curday", g@"curval", 
           g@"runits", g@"dir",g@"yaw", b];
  goalput[gs, "roadall" -> genJSON[rr]]]

(* TODO: canonicalize road matrix to merge redundant segments *)

End[]; (* Private context *)
EndPackage[];
