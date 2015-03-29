
BeginPackage["Wolfminder`"]


usr::usage = "Beeminder username";

key::usage = "\
Beeminder personal auth key -- https://www.beeminder.com/api/v1/auth_token.json\
";

userget;
goalget;
goalput;
allgoals;

Begin["`Private`"];  (*********************************************************)

(***************************** Utility functions ******************************)
ARGV = args = Drop[$CommandLine, 4];        (* Command line args.             *)
pr = WriteString["stdout", ##]&;            (* More                           *)
prn = pr[##, "\n"]&;                        (*  convenient                    *)
perr = WriteString["stderr", ##]&;          (*   print                        *)
perrn = perr[##, "\n"]&;                    (*    statements.                 *)
re = RegularExpression;                     (* I wish Wolfram weren't         *)
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


baseurl = "https://www.beeminder.com/api/v1/";

UPOCH = AbsoluteTime[DateObject[{1970,1,1, 0,0,0}, TimeZone->0]];

DIY = 365.25; (* this is what physicists use, eg, to define a light year *)
SID = 86400;  (* seconds in day *)
SECS = <| "y" -> DIY*SID,
          "m" -> DIY/12*SID,
          "w" -> 7*SID,
          "d" -> SID,
          "h" -> 3600 |>;

jsonify[x_] := ExportString[x, "JSON", "Compact"->True]

(* TODO: don't use querystring *)
userget::usage = "Return Associaion of user info"
userget[] := Association@Import[baseurl <> "users/me.json?auth_token=" <> key]

(* TODO: don't use querystring *)
goalget::usage = "Return a hash of goal parameters for goal slug g";
goalget[g_] := Association@Import[
  baseurl<>"users/me/goals/"<>g<>".json?auth_token="<>key, "JSON"]

(* Update goal with slug g with hash h *)
goalput[g_, h___] := URLFetch[baseurl<>"users/me/goals/"<>g<>".json", 
  "Method"->"PUT", "Parameters"->{"auth_token"->key, h}]

allgoals::usage = "Return a list of goal Associations"
allgoals[] := Association /@ ImportString[
   URLFetch[baseurl<>"users/me/goals.json", "Method"->"GET",
            "Parameters"->{"auth_token"->key}], "JSON"]

shdt[t_] := DateString[t + UPOCH, {"MonthNameShort", "Day"}]

shv[v_]      := If[v === Null, "", shn@v]
shr[r_, ru_] := If[r === Null, "", cat[shn@r, "/", ru]]

showroad[gs_] := With[{g = goalget[gs]},
  Grid[{{MatrixForm[{shdt@#1, shv@#2, shr[#3, g["runits"]]} & @@@ 
       g["roadall"]], Import[g["thumb_url"]]}}]]



(*
History-Destroying Retroratchet aka RETROratchet as opposed to Ratchet

Let b be the number of days of safety buffer you want.
MOAR/PHAT: tini,vini -> tcur+b-1,vcur
WEEN/RASH: tini,vini -> tcur,vcur+yaw*rcur*(b-1)
*)

(* Take a roadall, tcur/vcur, runits, dir, yaw, and the desired number of days 
   of safety buffer (b) and return a new roadall that goes from {tini,vini} to 
   tfin -- no rows except the {tfin,vfin,rfin} row. *)
rr0[{{tini_, vini_, _}, ___, {tfin_, vfin_, rfin_}}, tcur_, vcur_, runits_, 
    dir_, yaw_, b_] := With[ru = SECS[runits],
  If[dir*yaw > 0,
    {{tini,vini, Null}, {tfin, Null, (vcur-vini)/(tcur+b*SID-1-tini)*ru}},
    {{tini,vini, Null}, {tfin, Null, (vcur-vini)/(tcur-tini+yaw(b-1)*SID)*ru}}]]

(* Take a goal slug g and retroratchet it so it has b days of safety buffer *)
retrat[g_, b_] := Module[{h, rr}, 
  h = goalget[g];
  rr = rr0[h@"roadall", h@"curday", h@"curval", h@"runits", h@"dir",h@"yaw", b];
  goalput[g, "roadall" -> jsonify[rr]]]


End[]; (* Private context *)
EndPackage[];
