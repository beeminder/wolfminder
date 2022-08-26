BeginPackage["Wolfminder`"]

usr::usage = "Beeminder username";
key::usage = "\
Beeminder personal auth key -- https://www.beeminder.com/api/v1/auth_token.json\
";
userget;
goalget;
goalput;
allgoals;
dataget;
datapost;
dataput;
showroad;
parseJSON; (* Normally no need to expose JSON parsing *)
genJSON;

retrat; (* Work in progress *)

Begin["`Private`"];  (*********************************************************)

baseurl = "https://www.beeminder.com/api/v1/";

(***************************** Utility functions ******************************)
re = RegularExpression;                     (* I wish Wolframaic weren't      *)
EOF = EndOfFile;                            (*   so dang verbose!             *)
cat = StringJoin@@(ToString/@{##})&;        (* Like sprintf/strout in C/C++.  *)
eval = ToExpression[cat[##]]&;              (* Like eval in every other lang. *)
SetAttributes[each, HoldAll];               (* each[pattern, list, body]      *)
each[pat_, lst_List, bod_] :=               (*  converts pattern to body for  *)
  (Cases[Unevaluated@lst, pat:>bod]; Null); (*   each element of list.        *)
each[p_, l_, b_] := (Cases[l, p:>b]; Null); (*    (Warning: eats Return[]s.)  *)

SetAttributes[meach, HoldAll];              (* like each but monitor progress *)
meach[pat_, lst_, bod_] := Module[{n = Length[lst], i = 0},
  Monitor[each[pat, lst, i++; bod], ProgressIndicator[i/n]]]
(******************************************************************************)

(* The builtin JSON parsing with ImportString fails on unicode strings.
   This seems to be more robust. Just make sure you trust the string to 
   actually be JSON since the trick here is to do some simple substitutions and
   then eval it. 
   Cf http://stackoverflow.com/questions/2633003/parsing-and-generating-json *)
parseJSON[json_String] := With[{tr = {"["     -> "(*_MAGIC__[__*){",
                                      "]"     -> "(*_MAGIC__]__*)}",
                                      ":"     -> "(*_MAGIC__:__*)->",
                                      "true"  -> "(*_MAGIC__t__*)True",
                                      "false" -> "(*_MAGIC__f__*)False",
                                      "null"  -> "(*_MAGIC__n__*)Null",
                                      "e"     -> "(*_MAGIC__e__*)*10^",
                                      "E"     -> "(*_MAGIC__E__*)*10^"}},
  eval@StringReplace[cat@FullForm@eval[StringReplace[json, tr]], Reverse/@tr]]

(* parseJSON[json_String] := ImportString[json, "JSON"] *)

(* Similar problems with the builtin ExportString[_, "JSON"] *)
jnum[x_] := StringReplace[
  ToString@NumberForm[N@x, ExponentFunction->(Null&)], re@"\\.$"->""]
genJSON[s_String]  := s (* gross special case to not do "\"foo\"" *)
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
(*  [probably safe to kill this by now]
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
shns[x_, sf_:10] := shn[x, sf, {"-","+"}] (* signed version *)

userget::usage = "Return Association of user info";
userget[] := Association@parseJSON@URLFetch[
  baseurl<>"users/me.json", 
  "Method"->"GET", 
  "Parameters"->{"auth_token"->key}]

goalget::usage = "Return a hash of goal parameters for goal with goalname g";
goalget[g_] := Association@parseJSON@URLFetch[
  baseurl<>"users/me/goals/"<>g<>".json", 
  "Method"->"GET",
  "Parameters"->{"auth_token"->key}]

(* Update goal with goalname g with hash h *)
goalput[g_, h___] := Association@parseJSON@URLFetch[
  baseurl<>"users/me/goals/"<>g<>".json", 
  "Method"->"PUT", 
  "Parameters"->{"auth_token"->key, Splice[#1->genJSON[#2]& @@@ {h}]}]

allgoals::usage = "Return a list of goal Associations"
allgoals[] := Association /@ parseJSON@URLFetch[
  baseurl<>"users/me/goals.json",
  "Method"->"GET", 
  "Parameters"->{"auth_token"->key}]

shdt[Null] := ""
shdt[t_] := DateString[FromUnixTime[t], {"MonthNameShort", "Day"}]

shv[v_]      := If[v === Null, "", shn@v]
shr[r_, ru_] := If[r === Null, "", cat[shn@r, "/", ru]]

(* Helper for showroad that takes the road matrix and runits *)
showroad0[road_, ru_] := MatrixForm[{shdt@#1, shv@#2, shr[#3, ru]}& @@@ road]

(* Return human-friendly representation of a goal's YBR *)
showroad[gs_String] := With[{g = goalget[gs]},
  Grid[{{showroad0[g@"roadall", g@"runits"], Import[g@"thumb_url"]}}]]

dataget::usage = "Return a list of datapoint Associations";
dataget[g_] := Association /@ parseJSON@URLFetch[
  baseurl<>"users/me/goals/"<>g<>"/datapoints.json",
  "Parameters"->{"auth_token"->key}]

datapost::usage = "Add list of datapoints d to a goal g";
datapost[g_, d_] := Association@parseJSON@URLFetch[
  baseurl<>"users/me/goals/"<>g<>"/datapoints/create_all.json",
  "Method"->"POST", 
  "Parameters"->{"auth_token"->key, "datapoints"->genJSON[d]}]

dataput::usage = "Update a datapoint with the given id to have timestamp t etc";
dataput[g_, id_, t_, v_, c_] := Association@parseJSON@URLFetch[
  baseurl<>"users/me/goals/"<>g<>"/datapoints/"<>id<>".json",
  "Method"->"PUT", 
  "Parameters"->{"auth_token"->key, 
    "timestamp" -> genJSON[t],  (* The gross special case in genJSON means *)
    "value"     -> genJSON[v],  (* genJSON[c] here would be the same as not *)
    "comment"   -> c            (* calling genJSON and just using c. *)
  }]

(*

odomifyHelper[{prev_,offset_}, next_]:= {next, offset+If[prev>next==0, prev, 0]}
odomify[l_List] := l + Rest[FoldList[odomifyHelper, {-Infinity,0}, l]][[All,2]]

(* Turn the odometer-style goal with goalname g into a normal kyoomy do-more 
   goal by replacing each datapoint value with the delta from the previous 
   (aggregated) datapoint value. *)
kyoomify[g_String] := Module[{go, k, a, o, data, vals, deltas, gq},
  go = goalget[g];
  k = go["kyoom"];
  a = go["aggday"];
  o = go["odom"];
  data = Reverse[dataget[g]];
  vals = #["value"]& /@ data;
  deltas = Differences[vals];
  If[k, Return["ERROR: Goal is already kyoom"]];
  If[a =!= "last", Return[
    "ERROR: This goal has aggday='"<>a<>"' but only 'last' is supported"]];
  If[o, vals = odomify[vals]];
  If[Min[deltas] < 0, Return[
    "ERROR: Data must be monotone (other than odometer resets)"]];
  gq = goalput[g, "kyoom" -> True, "aggday" -> "sum", "odom" -> False];
  If[gq["kyoom"] =!= True || gq["aggday"] =!= "sum" || gq["odom"] =!= False, 
    Return["UNKNOWN ERROR: Failed to update goal params (kyoom/aggday/odom)"]];
  meach[{d_, delta_}, Transpose[{Rest[data], deltas}],
    dataput[g, d["id"], d["timestamp"], delta, 
            cat[d["comment"], "(*", d["value"], "*)"]]]]

*)

(*
History-Destroying Retroratchet aka RETROratchet as opposed to Ratchet

Let b be the number of days of safety buffer you want.
MOAR/PHAT: tini,vini -> tcur+b-1,vcur
WEEN/RASH: tini,vini -> tcur,vcur+yaw*rcur*(b-1)
*)

(* Take start and end points of a road, tcur/vcur, runits, dir, yaw, and the 
   desired number of days of safety buffer (b) and return a new roadall that 
   goes from {tini,vini} to tfin -- no rows except the {tfin,vfin,rfin} row. *)
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

(* would be nice to canonicalize graph matrix to merge redundant segments *)

End[]; (* Private context *)
EndPackage[];
