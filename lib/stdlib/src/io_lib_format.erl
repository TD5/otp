%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(io_lib_format).
-moduledoc false.

%% Formatting functions of io library.

-export([fwrite/2,fwrite/3,fwrite_g/1,indentation/2,scan/2,unscan/1,
         build/1, build/2]).

%%  Format the arguments in Args after string Format. Just generate
%%  an error if there is an error in the arguments.
%%
%%  To do the printing command correctly we need to calculate the
%%  current indentation for everything before it. This may be very
%%  expensive, especially when it is not needed, so we first determine
%%  if, and for how long, we need to calculate the indentations. We do
%%  this by first collecting all the control sequences and
%%  corresponding arguments, then counting the print sequences and
%%  then building the output.  This method has some drawbacks, it does
%%  two passes over the format string and creates more temporary data,
%%  and it also splits the handling of the control characters into two
%%  parts.

-spec fwrite(Format, Data) -> io_lib:chars() when
      Format :: io:format(),
      Data :: [term()].

% Shortcuts for very common formats
fwrite("~p", [Term]) ->
    Options = [{chars_limit, -1},
               {column, 1},
               {line_length, 80},
               {depth, -1},
               {encoding, latin1},
               {strings, true},
               {maps_order, undefined}],
    io_lib_pretty:print(Term, Options);
fwrite("~0p", [Term]) ->
    Options = [{chars_limit, -1},
               {column, 1},
               {line_length, 0},
               {depth, -1},
               {encoding, latin1},
               {strings, true},
               {maps_order, undefined}],
    io_lib_pretty:print(Term, Options);
fwrite("~tp", [Term]) ->
    Options = [{chars_limit, -1},
               {column, 1},
               {line_length, 80},
               {depth, -1},
               {encoding, unicode},
               {strings, true},
               {maps_order, undefined}],
    io_lib_pretty:print(Term, Options);
fwrite("~0tp", [Term]) ->
    Options = [{chars_limit, -1},
               {column, 1},
               {line_length, 0},
               {depth, -1},
               {encoding, unicode},
               {strings, true},
               {maps_order, undefined}],
    io_lib_pretty:print(Term, Options);
fwrite("~w", [Term]) ->
    Options = [{chars_limit, -1},
               {depth, -1},
               {encoding, latin1}],
    io_lib:write(Term,Options);
fwrite("~tw", [Term]) ->
    Options = [{chars_limit, -1},
               {depth, -1},
               {encoding, unicode}],
    io_lib:write(Term,Options);
fwrite(Format, Args) ->
    build(scan(Format, Args)).

-spec fwrite(Format, Data, Options) -> io_lib:chars() when
      Format :: io:format(),
      Data :: [term()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: io_lib:chars_limit().

fwrite(Format, Args, Options) ->
    build(scan(Format, Args), Options).

%% Build the output text for a pre-parsed format list.

-spec build(FormatList) -> io_lib:chars() when
      FormatList :: [char() | io_lib:format_spec()].

build(Cs) ->
    build(Cs, []).

-spec build(FormatList, Options) -> io_lib:chars() when
      FormatList :: [char() | io_lib:format_spec()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: io_lib:chars_limit().

build(Cs, []) ->
    % No options means we'll default to unlimited chars
    build_unlimited(Cs);
build(Cs, Options) ->
    CharsLimit = get_option(chars_limit, Options, -1),
    case CharsLimit of
        -1 ->
            build_unlimited(Cs);
        _ ->
            Res1 = build_small(Cs),
            {P, S, W, Other} = count_small(Res1),
            case P + S + W of
                0 ->
                    Res1;
                NumOfLimited ->
                    RemainingChars = sub(CharsLimit, Other),
                    build_limited(Res1, P, NumOfLimited, RemainingChars, 0)
            end
    end.

%% Parse all control sequences in the format string.

-spec scan(Format, Data) -> FormatList when
      Format :: io:format(),
      Data :: [term()],
      FormatList :: [char() | io_lib:format_spec()].

scan(Format, Args) when is_atom(Format) ->
    scan(atom_to_list(Format), Args);
scan(Format, Args) when is_binary(Format) ->
    scan(binary_to_list(Format), Args);
scan(Format, Args) ->
    collect(Format, Args).

%% Revert a pre-parsed format list to a plain character list and a
%% list of arguments.

-spec unscan(FormatList) -> {Format, Data} when
      FormatList :: [char() | io_lib:format_spec()],
      Format :: io:format(),
      Data :: [term()].

unscan(Cs) ->
    {print(Cs), args(Cs)}.

args([#{args := As, maps_order := O} | Cs]) when is_function(O, 2); O =:= reversed ->
    [O | As] ++ args(Cs);
args([#{args := As} | Cs]) ->
    As ++ args(Cs);
args([_C | Cs]) ->
    args(Cs);
args([]=Nil) ->
    Nil.

print([#{control_char := C, width := F, adjust := Ad, precision := P,
         pad_char := Pad, encoding := Encoding, strings := Strings
        } = Map | Cs]) ->
    MapsOrder = maps:get(maps_order, Map, undefined),
    print(C, F, Ad, P, Pad, Encoding, Strings, MapsOrder) ++ print(Cs);
print([C | Cs]) when is_integer(C) ->
    [C | print(Cs)];
print([]=Nil) ->
    Nil.

print(C, F, Ad, P, Pad, Encoding, Strings, MapsOrder) ->
    [$~] ++ print_field_width(F, Ad) ++ print_precision(P, Pad) ++
        print_pad_char(Pad) ++ print_encoding(Encoding) ++
        print_strings(Strings) ++ print_maps_order(MapsOrder) ++
        [C].

print_field_width(none, _Ad) -> "";
print_field_width(F, left) -> int_to_str(-F);
print_field_width(F, right) -> int_to_str(F).

print_precision(none, $\s) -> "";
print_precision(none, _Pad) -> ".";  % pad must be second dot
print_precision(P, _Pad) -> [$. | int_to_str(P)].

print_pad_char($\s) -> ""; % default, no need to make explicit
print_pad_char(Pad) -> [$., Pad].

print_encoding(unicode) -> "t";
print_encoding(latin1) -> "".

print_strings(false) -> "l";
print_strings(true) -> "".

print_maps_order(undefined) -> "";
print_maps_order(ordered) -> "k";
print_maps_order(reversed) -> "K";
print_maps_order(CmpFun) when is_function(CmpFun, 2) -> "K".

% Optimise common cases
collect([$~,$n|Fmt1], Args) ->
    C = #{args => [], encoding => latin1, adjust => right,
        maps_order => undefined, width => none, strings => true,
        precision => none, pad_char => 32, control_char => 110
    },
    [C|collect(Fmt1,Args)];
collect([$~,$s|Fmt1], [S|Args1]) ->
    C = #{args => [S], encoding => latin1, adjust => right,
            maps_order => undefined, width => none,strings => true,
            precision => none, pad_char => 32, control_char => 115
        },
    [C|collect(Fmt1,Args1)];
collect([$~,$t,$s|Fmt1], [S|Args1]) ->
    C = #{args => [S], encoding => unicode, adjust => right,
            maps_order => undefined, width => none,strings => true,
            precision => none, pad_char => 32, control_char => 115
        },
    [C|collect(Fmt1,Args1)];
collect([$~,$B|Fmt1], [N|Args1]) ->
    C = #{args => [N], encoding => latin1, adjust => right,
            maps_order => undefined, width => none,strings => true,
            precision => none, pad_char => 32, control_char => 66
        },
    [C|collect(Fmt1,Args1)];
collect([$~,$b|Fmt1], [N|Args1]) ->
    C = #{args => [N], encoding => latin1, adjust => right,
            maps_order => undefined, width => none,strings => true,
            precision => none, pad_char => 32, control_char => 98
        },
    [C|collect(Fmt1,Args1)];
collect([$~,$p|Fmt1], [A|Args1]) ->
    C = #{args => [A], encoding => latin1, adjust => right,
            maps_order => undefined, width => none,strings => true,
            precision => none, pad_char => 32, control_char => 112
        },
    [C|collect(Fmt1,Args1)];
collect([$~,$t,$p|Fmt1], [A|Args1]) ->
    C = #{args => [A], encoding => unicode, adjust => right,
            maps_order => undefined, width => none,strings => true,
            precision => none, pad_char => 32, control_char => 112
        },
    [C|collect(Fmt1,Args1)];
collect([$~,$0,$p|Fmt1], [A|Args1]) ->
    C = #{args => [A], encoding => latin1, adjust => right,
            maps_order => undefined, width => 0, strings => true,
            precision => none, pad_char => 32, control_char => 112
        },
    [C|collect(Fmt1,Args1)];
collect([$~,$w|Fmt1], [A|Args1]) ->
    C = #{args => [A], encoding => latin1, adjust => right,
            maps_order => undefined, width => none,strings => true,
            precision => none, pad_char => 32, control_char => 119
        },
    [C|collect(Fmt1,Args1)];
collect([$~,$t,$w|Fmt1], [A|Args1]) ->
    C = #{args => [A], encoding => unicode, adjust => right,
            maps_order => undefined, width => none,strings => true,
            precision => none, pad_char => 32, control_char => 119
        },
    [C|collect(Fmt1,Args1)];

collect([$~|Fmt0], Args0) ->
    {C,Fmt1,Args1} = collect_cseq(Fmt0, Args0),
    [C|collect(Fmt1, Args1)];
collect([C|Fmt], Args) ->
    [C|collect(Fmt, Args)];
collect([]=Nil, []) -> Nil.

collect_cseq(Fmt0, Args0) ->
    {F,Ad,Fmt1,Args1} = field_width(Fmt0, Args0),
    {P,Fmt2,Args2} = precision(Fmt1, Args1),
    {Pad,Fmt3,Args3} = pad_char(Fmt2, Args2),
    Spec0 = #{width => F,
              adjust => Ad,
              precision => P,
              pad_char => Pad,
              encoding => latin1,
              strings => true,
              maps_order => undefined},
    {Spec1,Fmt4,Args4} = modifiers(Fmt3, Args3, Spec0),
    {C,As,Fmt5,Args5} = collect_cc(Fmt4, Args4),
    Spec2 = Spec1#{control_char => C, args => As},
    {Spec2,Fmt5,Args5}.

modifiers([$t|Fmt], Args, Spec) ->
    modifiers(Fmt, Args, Spec#{encoding => unicode});
modifiers([$l|Fmt], Args, Spec) ->
    modifiers(Fmt, Args, Spec#{strings => false});
modifiers([$k|Fmt], Args, Spec) ->
    modifiers(Fmt, Args, Spec#{maps_order => ordered});
modifiers([$K|Fmt], [MapsOrder | Args], Spec) ->
    modifiers(Fmt, Args, Spec#{maps_order => MapsOrder});
modifiers(Fmt, Args, Spec) ->
    {Spec, Fmt, Args}.

field_width([$-|Fmt0], Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(-F, Fmt, Args);
field_width(Fmt0, Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(F, Fmt, Args).

field_width(F, Fmt, Args) when F < 0 ->
    {-F,left,Fmt,Args};
field_width(F, Fmt, Args) when F >= 0 ->
    {F,right,Fmt,Args}.

precision([$.|Fmt], Args) ->
    field_value(Fmt, Args);
precision(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([$*|Fmt], [A|Args]) when is_integer(A) ->
    {A,Fmt,Args};
field_value([C|Fmt], Args) when is_integer(C), C >= $0, C =< $9 ->
    field_value([C|Fmt], Args, 0);
field_value(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([C|Fmt], Args, F) when is_integer(C), C >= $0, C =< $9 ->
    field_value(Fmt, Args, 10*F + (C - $0));
field_value(Fmt, Args, F) ->		%Default case
    {F,Fmt,Args}.

pad_char([$.,$*|Fmt], [Pad|Args]) -> {Pad,Fmt,Args};
pad_char([$.,Pad|Fmt], Args) -> {Pad,Fmt,Args};
pad_char(Fmt, Args) -> {$\s,Fmt,Args}.

%% collect_cc([FormatChar], [Argument]) ->
%%	{Control,[ControlArg],[FormatChar],[Arg]}.
%%  Here we collect the argments for each control character.
%%  Be explicit to cause failure early.

collect_cc([C=$w|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$p|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$W|Fmt], [A,Depth|Args]) -> {C,[A,Depth],Fmt,Args};
collect_cc([C=$P|Fmt], [A,Depth|Args]) -> {C,[A,Depth],Fmt,Args};
collect_cc([C=$s|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$e|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$f|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$g|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$b|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$B|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$x|Fmt], [A,Prefix|Args]) -> {C,[A,Prefix],Fmt,Args};
collect_cc([C=$X|Fmt], [A,Prefix|Args]) -> {C,[A,Prefix],Fmt,Args};
collect_cc([C=$+|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$#|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$c|Fmt], [A|Args]) -> {C,[A],Fmt,Args};
collect_cc([C=$~|Fmt], []=Nil) -> {C,Nil,Fmt,Nil};
collect_cc([C=$n|Fmt], []=Nil) -> {C,Nil,Fmt,Nil};
collect_cc([C=$~|Fmt], [_|_]=Args) -> {C,[],Fmt,Args};
collect_cc([C=$n|Fmt], [_|_]=Args) -> {C,[],Fmt,Args};
collect_cc([C=$i|Fmt], [A|Args]) -> {C,[A],Fmt,Args}.

%% count_small([ControlC]) -> Count.
%%  Count the number of big (pPwWsS) print requests and
%%  number of characters of other print (small) requests.

count_small(Cs) ->
    count_small(Cs, #{p => 0, s => 0, w => 0, other => 0}).

% Pre-existing issue where we potentially pass a binary to io_lib:chars_length/1
-dialyzer({nowarn_function, [count_small/2]}).

count_small([#{control_char := $p}|Cs], #{p := P} = Cnts) ->
    count_small(Cs, Cnts#{p := P + 1});
count_small([#{control_char := $P}|Cs], #{p := P} = Cnts) ->
    count_small(Cs, Cnts#{p := P + 1});
count_small([#{control_char := $w}|Cs], #{w := W} = Cnts) ->
    count_small(Cs, Cnts#{w := W + 1});
count_small([#{control_char := $W}|Cs], #{w := W} = Cnts) ->
    count_small(Cs, Cnts#{w := W + 1});
count_small([#{control_char := $s}|Cs], #{w := W} = Cnts) ->
    count_small(Cs, Cnts#{w := W + 1});
count_small([[]|Cs], #{other := _} = Cnts) ->
    count_small(Cs, Cnts);
count_small([[_|_]=S|Cs], #{other := Other} = Cnts) ->
    count_small(Cs, Cnts#{other := Other + io_lib:chars_length(S)});
count_small([S|Cs], #{other := Other} = Cnts) when is_binary(S) ->
    count_small(Cs, Cnts#{other := Other + io_lib:chars_length(S)});
count_small([C|Cs], #{other := Other} = Cnts) when is_integer(C) ->
    count_small(Cs, Cnts#{other := Other + 1});
count_small([], #{p := P, s := S, w := W, other := Other}) ->
    {P, S, W, Other}.

count_ps(Cs) ->
    count_ps(Cs, 0).

count_ps([#{control_char := $p}|Cs], Acc) ->
    count_ps(Cs, Acc+1);
count_ps([#{control_char := $P}|Cs], Acc) ->
    count_ps(Cs, Acc+1);
count_ps([[]|Cs], Acc) ->
    count_ps(Cs, Acc);
count_ps([[_|_]=S|Cs], Acc) ->
    count_ps(Cs, count_ps(S,Acc));
count_ps([_|Cs], Acc) ->
    count_ps(Cs, Acc);
count_ps([], Acc) ->
    Acc.

%% build_small([Control]) -> io_lib:chars().
%%  Interpret the control structures, but only the small ones.
%%  The big ones are saved for later.
%% build_limited([Control], NumberOfPps, NumberOfLimited,
%%               CharsLimit, Indentation)
%%  Interpret the control structures. Count the number of print
%%  remaining and only calculate indentation when necessary. Must also
%%  be smart when calculating indentation for characters in format.

build_small([#{control_char := C, args := As, width := F, adjust := Ad,
               precision := P, pad_char := Pad, encoding := Enc}=CC | Cs]) ->
    case control_small(C, As, F, Ad, P, Pad, Enc) of
        not_small -> [CC | build_small(Cs)];
        S -> lists:flatten(S, build_small(Cs))
    end;
build_small([C|Cs]) -> [C|build_small(Cs)];
build_small([]=Nil) -> Nil.

build_limited([#{control_char := C, args := As, width := F, adjust := Ad,
                 precision := P, pad_char := Pad, encoding := Enc,
                 strings := Str} = Map | Cs],
              NumOfPs0, Count0, MaxLen0, I) ->
    Ord = maps:get(maps_order, Map, undefined),
    MaxChars = if
                   MaxLen0 < 0 -> MaxLen0;
                   true -> MaxLen0 div Count0
               end,
    S = control_limited(C, As, F, Ad, P, Pad, Enc, Str, Ord, MaxChars, I),
    NumOfPs = decr_pc(C, NumOfPs0),
    Count = Count0 - 1,
    MaxLen = if
                 MaxLen0 < 0 -> % optimization
                     MaxLen0;
                 true ->
                     Len = io_lib:chars_length(S),
                     sub(MaxLen0, Len)
             end,
    if
	NumOfPs > 0 -> [S|build_limited(Cs, NumOfPs, Count,
                                        MaxLen, indentation(S, I))];
	true -> [S|build_limited(Cs, NumOfPs, Count, MaxLen, I)]
    end;
build_limited([C=$\n|Cs], NumOfPs, Count, MaxLen, _I) ->
    [C|build_limited(Cs, NumOfPs, Count, MaxLen, 0)];
build_limited([C=$\t|Cs], NumOfPs, Count, MaxLen, I) ->
    [C|build_limited(Cs, NumOfPs, Count, MaxLen, ((I + 8) div 8) * 8)];
build_limited([C|Cs], NumOfPs, Count, MaxLen, I) ->
    [C|build_limited(Cs, NumOfPs, Count, MaxLen, I+1)];
build_limited([]=Nil, _, _, _, _) -> Nil.

decr_pc($p, Pc) -> Pc - 1;
decr_pc($P, Pc) -> Pc - 1;
decr_pc(_, Pc) -> Pc.

% Avoid extra allocations and unnecessary wrapping when only one/zero element needs
% to be built
build_unlimited([]=Nil) ->
    Nil;
build_unlimited([#{control_char := C, args := As, width := F, adjust := Ad,
                 precision := P, pad_char := Pad, encoding := Enc,
                 strings := Str}=Map]) ->
    Ord = maps:get(maps_order, Map, undefined),
    case control_unlimited_big(C, As, F, Ad, P, Pad, Enc, Str, Ord, 0) of
        {small, Small} -> lists:flatten(Small);
        Big -> Big
    end;
build_unlimited([#{control_char := C, args := As, width := F, adjust := Ad,
                 precision := P, pad_char := Pad, encoding := Enc}]) ->
    S = control_unlimited_small(C, As, F, Ad, P, Pad, Enc),
    lists:flatten(S);
build_unlimited(Cs) ->
    build_unlimited_1(Cs, [], count_ps(Cs), 0).

% For backwards compatibility, any prefix built entirely of "small" format
% strings must be flattened. We attempt to do that here, but with fewer
% intermediate lists than doing it naively:
% Whilst we have only formatted "small" strings, we accumulate a list of
% the sub-components, which we reverse and flatten at once at the end of
% the input, or when we hit a "big" string, at which point we give up
% on trying to collect and efficiently flatten the output. When we flatten,
% we directly prepend to the tail of the list, rather than flattening, then
% prepending afterwards.
% Building up an accumulated list of small formatted strings shouldn't cause
% an unreasonable spike in memory usage, since the "small" strings are limited
% by format string length, and the limited length of atoms, numbers, etc.
build_unlimited_1([#{control_char := C, args := As, width := F, adjust := Ad,
                 precision := P, pad_char := Pad, encoding := Enc,
                 strings := Str} = Map | Cs], SmallAcc, NumOfPs0, I) ->
    Ord = maps:get(maps_order, Map, undefined),
    {S, IsSmall} =
        case control_unlimited_big(C, As, F, Ad, P, Pad, Enc, Str, Ord, I) of
            {small, Small} -> {Small,true};
            Big -> {Big,false}
        end,
    NumOfPs = decr_pc(C, NumOfPs0),
    Ind =
        case NumOfPs of
            N when N > 0 -> indentation(S, I);
            _ -> I
        end,
    case {SmallAcc, IsSmall} of
        {[]=Nil,false} ->
            [S|build_unlimited_1(Cs, Nil, NumOfPs, Ind)];
        {[],true} ->
            build_unlimited_1(Cs, [S], NumOfPs, Ind);
        {L,false} ->
            append_rev_chars(L, [S|build_unlimited_1(Cs, [], NumOfPs, Ind)]);
        {L,true} ->
            build_unlimited_1(Cs, [S|L], NumOfPs, Ind)
    end;
build_unlimited_1([#{control_char := C, args := As, width := F, adjust := Ad,
                 precision := P, pad_char := Pad, encoding := Enc} | Cs], SmallAcc, NumOfPs0, I) ->
    S = control_unlimited_small(C, As, F, Ad, P, Pad, Enc),
    NumOfPs = decr_pc(C, NumOfPs0),
    Ind =
        case NumOfPs of
            N when N > 0 -> indentation(S, I);
            _ -> I
        end,
    build_unlimited_1(Cs, [S|SmallAcc], NumOfPs, Ind);
build_unlimited_1([C=$\n|Cs], []=Nil, NumOfPs, _I) ->
    [C|build_unlimited_1(Cs, Nil, NumOfPs, 0)];
build_unlimited_1([C=$\t|Cs], []=Nil, NumOfPs, I) ->
    [C|build_unlimited_1(Cs, Nil, NumOfPs, ((I + 8) div 8) * 8)];
build_unlimited_1([C|Cs], []=Nil, NumOfPs, I) ->
    [C|build_unlimited_1(Cs, Nil, NumOfPs, I+1)];
build_unlimited_1([], []=Nil, _, _) ->
    Nil;
build_unlimited_1([C=$\n|Cs], SmallAcc, NumOfPs, _I) ->
    append_rev_chars(SmallAcc, [C|build_unlimited_1(Cs, [], NumOfPs, 0)]);
build_unlimited_1([C=$\t|Cs], SmallAcc, NumOfPs, I) ->
    append_rev_chars(SmallAcc, [C|build_unlimited_1(Cs, [], NumOfPs, ((I + 8) div 8) * 8)]);
build_unlimited_1([C|Cs], SmallAcc, NumOfPs, I) ->
    append_rev_chars(SmallAcc, [C|build_unlimited_1(Cs, [], NumOfPs, I+1)]);
build_unlimited_1([], [_|_]=SmallAcc, _, _) ->
    append_rev_chars(SmallAcc).

%%  Calculate the indentation of the end of a string given its start
%%  indentation. We assume tabs at 8 cols.

-spec indentation(String, StartIndent) -> integer() when
      String :: io_lib:chars(),
      StartIndent :: integer().

indentation([], I) -> I;
indentation([$\n|Cs], _I) -> indentation(Cs, 0);
indentation([$\t|Cs], I) -> indentation(Cs, ((I + 8) div 8) * 8);
indentation([C|Cs], I) when is_integer(C) ->
    indentation(Cs, I+1);
indentation([C|Cs], I) ->
    indentation(Cs, indentation(C, I)).

%% control_small(FormatChar, [Argument], FieldWidth, Adjust, Precision,
%%               PadChar, Encoding) -> String
%% control_limited(FormatChar, [Argument], FieldWidth, Adjust, Precision,
%%                 PadChar, Encoding, StringP, ChrsLim, Indentation) -> String
%%  These are the dispatch functions for the various formatting controls.

control_small(C=$~, [], F, Adj, P, Pad, _Enc) -> char(C, F, Adj, P, Pad);
control_small($n, [], F, Adj, P, Pad, _Enc) -> newline(F, Adj, P, Pad);
control_small($i, [_A], _F, _Adj, _P, _Pad, _Enc) -> [];
control_small($s, [A], F, Adj, P, Pad, latin1=Enc) when is_atom(A) ->
    L = atom_to_list(A),
    string(L, F, Adj, P, Pad, Enc);
control_small($s, [A], F, Adj, P, Pad, unicode=Enc) when is_atom(A) ->
    string(atom_to_list(A), F, Adj, P, Pad, Enc);
control_small($e, [A], F, Adj, P, Pad, _Enc) when is_float(A) ->
    fwrite_e(A, F, Adj, P, Pad);
control_small($f, [A], F, Adj, P, Pad, _Enc) when is_float(A) ->
    fwrite_f(A, F, Adj, P, Pad);
control_small($g, [A], F, Adj, P, Pad, _Enc) when is_float(A) ->
    fwrite_g(A, F, Adj, P, Pad);
control_small($b, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, true);
control_small($B, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, false);
control_small($x, [A,Prefix], F, Adj, P, Pad, _Enc) when is_integer(A),
                                                         is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), true);
control_small($x, [A,Prefix], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, true);
control_small($X, [A,Prefix], F, Adj, P, Pad, _Enc) when is_integer(A),
                                                         is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), false);
control_small($X, [A,Prefix], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, false);
control_small($+, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    Base = base(P),
    Prefix = [int_to_str(Base), $#],
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, true);
control_small(C=$#, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    Base = base(P),
    Prefix = [int_to_str(Base), C],
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, false);
control_small($c, [A], F, Adj, P, Pad, unicode) when is_integer(A) ->
    char(A, F, Adj, P, Pad);
control_small($c, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    char(A band 255, F, Adj, P, Pad);
control_small(_C, _As, _F, _Adj, _P, _Pad, _Enc) -> not_small.

control_limited($s, [L0], F, Adj, P, Pad, latin1=Enc, _Str, _Ord, CL, _I) ->
    L = iolist_to_chars(L0, F, CL),
    string(L, limit_field(F, CL), Adj, P, Pad, Enc);
control_limited($s, [L0], F, Adj, P, Pad, unicode=Enc, _Str, _Ord, CL, _I) ->
    L = cdata_to_chars(L0, F, CL),
    uniconv(string(L, limit_field(F, CL), Adj, P, Pad, Enc));
control_limited($w, [A], F, Adj, P, Pad, Enc, _Str, undefined, -1, _I) ->
    Chars = io_lib:write(A, [
        {encoding, Enc}
        % These are the defaults, so no need to pass them explicitly
        % {depth, -1},
        % {chars_limit, -1},
        % {maps_order, undefined}
    ]),
    term(Chars, F, Adj, P, Pad);
control_limited($w, [A], F, Adj, P, Pad, Enc, _Str, Ord, CL, _I) ->
    Chars = io_lib:write(A, [
        % {depth, -1}, % Default depth is -1
        {encoding, Enc},
        {chars_limit, CL},
        {maps_order, Ord}
    ]),
    term(Chars, F, Adj, P, Pad);
control_limited($p, [A], F, Adj, P, Pad, Enc, Str, Ord, CL, I) ->
    print(A, -1, F, Adj, P, Pad, Enc, Str, Ord, CL, I);
control_limited($W, [A,Depth], F, Adj, P, Pad, Enc, _Str, Ord, CL, _I)
           when is_integer(Depth) ->
    Chars = io_lib:write(A, [
        {depth, Depth},
        {encoding, Enc},
        {chars_limit, CL},
        {maps_order, Ord}
    ]),
    term(Chars, F, Adj, P, Pad);
control_limited($P, [A,Depth], F, Adj, P, Pad, Enc, Str, Ord, CL, I)
           when is_integer(Depth) ->
    print(A, Depth, F, Adj, P, Pad, Enc, Str, Ord, CL, I).

-define(validate_arg(Guard,Arg),
    if not Guard(Arg) -> throw(badarg); true -> ok end
).

% No chars limit
control_unlimited_big($s, [L0], F, Adj, P, Pad, latin1=Enc, _Str, _Ord, _I) ->
    case L0 of
        A when is_atom(A) ->
            LA = atom_to_list(A),
            io_lib:printable_latin1_list(LA) orelse throw(badarg),
            {small,string(LA, F, Adj, P, Pad, Enc)};
        _ ->
            L = iolist_to_chars(L0),
            string(L, F, Adj, P, Pad, Enc)
        end;
control_unlimited_big($s, [L0], F, Adj, P, Pad, unicode=Enc, _Str, _Ord, _I) ->
    case L0 of
        A when is_atom(A) ->
            {small,string(atom_to_list(A), F, Adj, P, Pad, Enc)};
        _ ->
            L = cdata_to_chars(L0),
            uniconv(string(L, F, Adj, P, Pad, Enc))
    end;
control_unlimited_big($w, [A], F, Adj, P, Pad, Enc, _Str, undefined, _I) ->
    Chars = io_lib:write(A, [
        {encoding, Enc}
        % These are the defaults, so no need to pass them explicitly
        % {depth, -1},
        % {maps_order, undefined}
    ]),
    term(Chars, F, Adj, P, Pad);
control_unlimited_big($w, [A], F, Adj, P, Pad, Enc, _Str, Ord, _I) ->
    Chars = io_lib:write(A, [
        % {depth, -1}, % Default depth is -1
        {encoding, Enc},
        {maps_order, Ord}
    ]),
    term(Chars, F, Adj, P, Pad);
control_unlimited_big($p, [A], F, Adj, P, Pad, Enc, Str, Ord, I) ->
    print_unlimited(A, -1, F, Adj, P, Pad, Enc, Str, Ord, I);
control_unlimited_big($W, [A,Depth], F, Adj, P, Pad, Enc, _Str, Ord, _I) ->
    ?validate_arg(is_integer,Depth),
    Chars = io_lib:write(A, [
        {depth, Depth},
        {encoding, Enc},
        {maps_order, Ord}
    ]),
    term(Chars, F, Adj, P, Pad);
control_unlimited_big($P, [A,Depth], F, Adj, P, Pad, Enc, Str, Ord, I) ->
    ?validate_arg(is_integer,Depth),
    print_unlimited(A, Depth, F, Adj, P, Pad, Enc, Str, Ord, I);
control_unlimited_big(C, Args, F, Adj, P, Pad, Enc, _Str, _Ord, _I) ->
    {small,control_unlimited_small(C, Args, F, Adj, P, Pad, Enc)}.

control_unlimited_small($s, [L0], F, Adj, P, Pad, latin1=Enc) ->
    case L0 of
        A when is_atom(A) ->
            LA = atom_to_list(A),
            io_lib:printable_latin1_list(LA) orelse throw(badarg),
            string(LA, F, Adj, P, Pad, Enc);
        _ ->
            L = iolist_to_chars(L0),
            string(L, F, Adj, P, Pad, Enc)
        end;
control_unlimited_small($s, [L0], F, Adj, P, Pad, unicode=Enc) ->
    case L0 of
        A when is_atom(A) ->
            string(atom_to_list(A), F, Adj, P, Pad, Enc);
        _ ->
            L = cdata_to_chars(L0),
            uniconv(string(L, F, Adj, P, Pad, Enc))
    end;
control_unlimited_small(C=$~, [], F, Adj, P, Pad, _Enc) ->
    char(C, F, Adj, P, Pad);
control_unlimited_small($n, [], F, Adj, P, Pad, _Enc) ->
    newline(F, Adj, P, Pad);
control_unlimited_small($i, [_A], _F, _Adj, _P, _Pad, _Enc) ->
    [];
control_unlimited_small($e, [A], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_float,A),
    fwrite_e(A, F, Adj, P, Pad);
control_unlimited_small($f, [A], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_float,A),
    fwrite_f(A, F, Adj, P, Pad);
control_unlimited_small($g, [A], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_float,A),
    fwrite_g(A, F, Adj, P, Pad);
control_unlimited_small($b, [A], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_integer,A),
    unprefixed_integer(A, F, Adj, base(P), Pad, true);
control_unlimited_small($B, [A], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_integer,A),
    unprefixed_integer(A, F, Adj, base(P), Pad, false);
control_unlimited_small($x, [A,Prefix], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_integer,A),
    if
        is_atom(Prefix) ->
            prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), true);
        true ->
            true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
            prefixed_integer(A, F, Adj, base(P), Pad, Prefix, true)
    end;
control_unlimited_small($X, [A,Prefix], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_integer,A),
    if
        is_atom(Prefix) ->
            prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), false);
        true ->
            true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
            prefixed_integer(A, F, Adj, base(P), Pad, Prefix, false)
    end;
control_unlimited_small($+, [A], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_integer,A),
    Base = base(P),
    Prefix = [int_to_str(Base), $#],
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, true);
control_unlimited_small(C=$#, [A], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_integer,A),
    Base = base(P),
    Prefix = [int_to_str(Base), C],
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, false);
control_unlimited_small($c, [A], F, Adj, P, Pad, unicode) ->
    ?validate_arg(is_integer,A),
    char(A, F, Adj, P, Pad);
control_unlimited_small($c, [A], F, Adj, P, Pad, _Enc) ->
    ?validate_arg(is_integer,A),
    char(A band 255, F, Adj, P, Pad).

-ifdef(UNICODE_AS_BINARIES).
uniconv(C) ->
    unicode:characters_to_binary(C,unicode).
-else.
uniconv(C) ->
    C.
-endif.
%% Default integer base
base(none) ->
    10;
base(B) when is_integer(B) ->
    B.

%% term(TermList, Field, Adjust, Precision, PadChar)
%%  Output the characters in a term.
%%  Adjust the characters within the field if length less than Max padding
%%  with PadChar.

term(T, none, _Adj, none, _Pad) -> T;
term(T, none, Adj, P, Pad) -> term(T, P, Adj, P, Pad);
term(T, F, Adj, P0, Pad) ->
    L = io_lib:chars_length(T),
    P = erlang:min(L, case P0 of none -> F; _ -> min(P0, F) end),
    if
	L > P ->
	    adjust(chars($*, P), chars(Pad, F-P), Adj);
	F >= P ->
	    adjust(T, chars(Pad, F-L), Adj)
    end.

%% print(Term, Depth, Field, Adjust, Precision, PadChar, Encoding,
%%       Indentation)
%% Print a term. Field width sets maximum line length, Precision sets
%% initial indentation.

print(T, D, none, Adj, P, Pad, E, Str, Ord, ChLim, I) ->
    print(T, D, 80, Adj, P, Pad, E, Str, Ord, ChLim, I);
print(T, D, F, Adj, none, Pad, E, Str, Ord, ChLim, I) ->
    print(T, D, F, Adj, I+1, Pad, E, Str, Ord, ChLim, I);
print(T, D, F, right, P, _Pad, Enc, Str, Ord, ChLim, _I) ->
    Options = [{chars_limit, ChLim},
               {column, P},
               {line_length, F},
               {depth, D},
               {encoding, Enc},
               {strings, Str},
               {maps_order, Ord}],
    io_lib_pretty:print(T, Options).

print_unlimited(T, D, none, Adj, P, Pad, E, Str, Ord, I) ->
    print_unlimited(T, D, 80, Adj, P, Pad, E, Str, Ord, I);
print_unlimited(T, D, F, Adj, none, Pad, E, Str, Ord, I) ->
    print_unlimited(T, D, F, Adj, I+1, Pad, E, Str, Ord, I);
print_unlimited(T, D, F, right, P, _Pad, Enc, Str, Ord, _I) ->
    UnlimitedChars={chars_limit, -1},
    Options =
        [UnlimitedChars,
         {column, P},
         {line_length, F},
         {depth, D},
         {encoding, Enc},
         {strings, Str},
         {maps_order, Ord}],
    io_lib_pretty:print(T, Options).

%% fwrite_e(Float, Field, Adjust, Precision, PadChar)

fwrite_e(Fl, none, Adj, none, Pad) ->		%Default values
    fwrite_e(Fl, none, Adj, 6, Pad);
fwrite_e(Fl, none, _Adj, P, _Pad) when P >= 2 ->
    float_e(Fl, float_data(Fl), P);
fwrite_e(Fl, F, Adj, none, Pad) ->
    fwrite_e(Fl, F, Adj, 6, Pad);
fwrite_e(Fl, F, Adj, P, Pad) when P >= 2 ->
    term(float_e(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_e(Fl, Fd, P) ->
    signbit(Fl) ++ abs_float_e(abs(Fl), Fd, P).

abs_float_e(_Fl, {Ds,E}, P) ->
    case float_man(Ds, 1, P-1) of
	{[$0|Fs],true} -> [[$1|Fs]|float_exp(E)];
	{Fs,false} -> [Fs|float_exp(E-1)]
    end.

%% float_man([Digit], Icount, Dcount) -> {[Char],CarryFlag}.
%%  Generate the characters in the mantissa from the digits with Icount
%%  characters before the '.' and Dcount decimals. Handle carry and let
%%  caller decide what to do at top.

float_man(Ds, 0, Dc) ->
    {Cs,C} = float_man(Ds, Dc),
    {[$.|Cs],C};
float_man([D|Ds], I, Dc) ->
    case float_man(Ds, I-1, Dc) of
	{Cs,true} when D =:= $9 -> {[$0|Cs],true};
	{Cs,true} -> {[D+1|Cs],false};
	{Cs,false} -> {[D|Cs],false}
    end;
float_man([], I, Dc) ->				%Pad with 0's
    {prepend_duplicates(I, $0, [$.|lists:duplicate(Dc, $0)]),false}.

float_man([D|_], 0) when D >= $5 -> {[],true};
float_man([_|_], 0) -> {[],false};
float_man([D|Ds], Dc) ->
    case float_man(Ds, Dc-1) of
	{Cs,true} when D =:= $9 -> {[$0|Cs],true};
	{Cs,true} -> {[D+1|Cs],false};
	{Cs,false} -> {[D|Cs],false}
    end;
float_man([], Dc) -> {lists:duplicate(Dc, $0),false}.	%Pad with 0's

%% float_exp(Exponent) -> [Char].
%%  Generate the exponent of a floating point number. Always include sign.

float_exp(E) when E >= 0 ->
    [$e,$+|int_to_str(E)];
float_exp(E) ->
    [$e|int_to_str(E)].

%% fwrite_f(FloatData, Field, Adjust, Precision, PadChar)

fwrite_f(Fl, none, Adj, none, Pad) ->		%Default values
    fwrite_f(Fl, none, Adj, 6, Pad);
fwrite_f(Fl, none, _Adj, P, _Pad) when P >= 1 ->
    float_f(Fl, float_data(Fl), P);
fwrite_f(Fl, F, Adj, none, Pad) ->
    fwrite_f(Fl, F, Adj, 6, Pad);
fwrite_f(Fl, F, Adj, P, Pad) when P >= 1 ->
    term(float_f(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_f(Fl, Fd, P) ->
    signbit(Fl) ++ abs_float_f(abs(Fl), Fd, P).

abs_float_f(Fl, {Ds,E}, P) when E =< 0 ->
    abs_float_f(Fl, {prepend_duplicates(-E+1, $0, Ds),1}, P);	%Prepend enough 0's
abs_float_f(_Fl, {Ds,E}, P) ->
    case float_man(Ds, E, P) of
	{Fs,true} -> "1" ++ Fs;			%Handle carry
	{Fs,false} -> Fs
    end.

%% signbit(Float) -> [$-] | []

signbit(Fl) when Fl < 0.0 -> [$-];
signbit(Fl) when Fl > 0.0 -> [];
signbit(Fl) ->
    case <<Fl/float>> of
        <<1:1,_:63>> -> [$-];
        _ -> []
    end.

%% float_data([FloatChar]) -> {[Digit],Exponent}

float_data(Fl) ->
    float_data(float_to_list(Fl), []).

float_data([$e|E], Ds) ->
    {lists:reverse(Ds),list_to_integer(E)+1};
float_data([D|Cs], Ds) when D >= $0, D =< $9 ->
    float_data(Cs, [D|Ds]);
float_data([_|Cs], Ds) ->
    float_data(Cs, Ds).

%%  Returns a correctly rounded string that converts to Float when
%%  read back with list_to_float/1.

-spec fwrite_g(float()) -> string().
fwrite_g(Float) ->
    float_to_list(Float, [short]).

%% fwrite_g(Float, Field, Adjust, Precision, PadChar)
%%  Use the f form if Float is >= 0.1 and < 1.0e4,
%%  and the prints correctly in the f form, else the e form.
%%  Precision always means the # of significant digits.

fwrite_g(Fl, F, Adj, none, Pad) ->
    fwrite_g(Fl, F, Adj, 6, Pad);
fwrite_g(Fl, F, Adj, P, Pad) when P >= 1 ->
    A = abs(Fl),
    E = if A < 1.0e-1 -> -2;
	   A < 1.0e0  -> -1;
	   A < 1.0e1  -> 0;
	   A < 1.0e2  -> 1;
	   A < 1.0e3  -> 2;
	   A < 1.0e4  -> 3;
	   true       -> fwrite_f
	end,
    if  P =< 1, E =:= -1;
	P-1 > E, E >= -1 ->
	    fwrite_f(Fl, F, Adj, P-1-E, Pad);
	P =< 1 ->
	    fwrite_e(Fl, F, Adj, 2, Pad);
	true ->
	    fwrite_e(Fl, F, Adj, P, Pad)
    end.


iolist_to_chars(Cs, F, CharsLimit) when CharsLimit < 0; CharsLimit >= F ->
    iolist_to_chars(Cs);
iolist_to_chars(Cs, _, CharsLimit) ->
    limit_iolist_to_chars(Cs, sub(CharsLimit, 3), [], normal). % three dots

-define(is_char(C), (is_integer(C) andalso C >= $\000 andalso C =< $\377)).

iolist_to_chars([]=Nil) ->
    Nil;
iolist_to_chars(B) when is_binary(B) ->
    binary_to_list(B);
iolist_to_chars([C1,C2,C3,C4,C5,C6,C7,C8]=Cs) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4),
        ?is_char(C5), ?is_char(C6), ?is_char(C7), ?is_char(C8) ->
    Cs;
iolist_to_chars([C1,C2,C3,C4,C5,C6,C7,C8|Cs]) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4),
        ?is_char(C5), ?is_char(C6), ?is_char(C7), ?is_char(C8) ->
    [C1,C2,C3,C4,C5,C6,C7,C8 | iolist_to_chars(Cs)];
iolist_to_chars([C1,C2,C3,C4,C5,C6,C7]=Cs) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4),
        ?is_char(C5), ?is_char(C6), ?is_char(C7) ->
    Cs;
iolist_to_chars([C1,C2,C3,C4,C5,C6|Cs]) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4),
        ?is_char(C5), ?is_char(C6) ->
    [C1,C2,C3,C4,C5,C6 | iolist_to_chars(Cs)];
iolist_to_chars([C1,C2,C3,C4,C5,C6]=Cs) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4),
        ?is_char(C5), ?is_char(C6) ->
    Cs;
iolist_to_chars([C1,C2,C3,C4,C5,C6|Cs]) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4),
        ?is_char(C5), ?is_char(C6) ->
    [C1,C2,C3,C4,C5,C6 | iolist_to_chars(Cs)];
iolist_to_chars([C1,C2,C3,C4,C5]=Cs) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4),
        ?is_char(C5) ->
    Cs;
iolist_to_chars([C1,C2,C3,C4,C5|Cs]) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4),
        ?is_char(C5) ->
    [C1,C2,C3,C4,C5 | iolist_to_chars(Cs)];
iolist_to_chars([C1,C2,C3,C4]=Cs) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4) ->
    Cs;
iolist_to_chars([C1,C2,C3,C4|Cs]) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3), ?is_char(C4) ->
    [C1,C2,C3,C4 | iolist_to_chars(Cs)];
iolist_to_chars([C1,C2,C3]=Cs) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3) ->
    Cs;
iolist_to_chars([C1,C2,C3|Cs]) when
        ?is_char(C1), ?is_char(C2), ?is_char(C3) ->
    [C1,C2,C3 | iolist_to_chars(Cs)];
iolist_to_chars([C1,C2]=Cs) when
        ?is_char(C1), ?is_char(C2) ->
    Cs;
iolist_to_chars([C1,C2|Cs]) when
        ?is_char(C1), ?is_char(C2) ->
    [C1,C2 | iolist_to_chars(Cs)];
iolist_to_chars([C]=Cs) when ?is_char(C) ->
    Cs;
iolist_to_chars([C|Cs]) when ?is_char(C) ->
    [C | iolist_to_chars(Cs)];
iolist_to_chars([I|Cs]) ->
    [iolist_to_chars(I) | iolist_to_chars(Cs)].

limit_iolist_to_chars(Cs, 0, S, normal) ->
    L = limit_iolist_to_chars(Cs, 4, S, final),
    case iolist_size(L) of
        N when N < 4 -> L;
        4 -> "..."
    end;
limit_iolist_to_chars(_Cs, 0, _S, final) -> [];
limit_iolist_to_chars([]=Nil, _Limit, [], _Mode) ->
    Nil;
limit_iolist_to_chars([], Limit, [Cs|S], Mode) ->
    limit_iolist_to_chars(Cs, Limit, S, Mode);
limit_iolist_to_chars([C|Cs], Limit, S, Mode) when C >= $\000, C =< $\377 ->
    [C | limit_iolist_to_chars(Cs, Limit - 1, S, Mode)];
limit_iolist_to_chars([I|Cs], Limit, S, Mode) ->
    limit_iolist_to_chars(I, Limit, [Cs|S], Mode);
limit_iolist_to_chars(B, Limit, S, Mode) when is_binary(B) ->
    case byte_size(B) of
        Sz when Sz > Limit ->
            {B1, B2} = split_binary(B, Limit),
            [binary_to_list(B1) | limit_iolist_to_chars(B2, 0, S, Mode)];
        Sz ->
            [binary_to_list(B) | limit_iolist_to_chars([], Limit-Sz, S, Mode)]
    end.

cdata_to_chars(Cs, F, CharsLimit) when CharsLimit < 0; CharsLimit >= F ->
    cdata_to_chars(Cs);
cdata_to_chars(Cs, _, CharsLimit) ->
    limit_cdata_to_chars(Cs, sub(CharsLimit, 3), normal). % three dots

-define(is_cp(C), is_integer(C), C >= 0).

cdata_to_chars([]=Nil) ->
    Nil;
cdata_to_chars(B) when is_binary(B) ->
    case catch unicode:characters_to_list(B) of
        L when is_list(L) -> L;
        _ -> binary_to_list(B)
    end;
cdata_to_chars([C1,C2,C3,C4,C5,C6,C7,C8]=Cs) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4),
        ?is_cp(C5), ?is_cp(C6), ?is_cp(C7), ?is_cp(C8) ->
    Cs;
cdata_to_chars([C1,C2,C3,C4,C5,C6,C7,C8|Cs]) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4),
        ?is_cp(C5), ?is_cp(C6), ?is_cp(C7), ?is_cp(C8) ->
    [C1,C2,C3,C4,C5,C6,C7,C8 | cdata_to_chars(Cs)];
cdata_to_chars([C1,C2,C3,C4,C5,C6,C7]=Cs) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4),
        ?is_cp(C5), ?is_cp(C6), ?is_cp(C7) ->
    Cs;
cdata_to_chars([C1,C2,C3,C4,C5,C6,C7|Cs]) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4),
        ?is_cp(C5), ?is_cp(C6), ?is_cp(C7) ->
    [C1,C2,C3,C4,C5,C6,C7 | cdata_to_chars(Cs)];
cdata_to_chars([C1,C2,C3,C4,C5,C6]=Cs) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4),
        ?is_cp(C5), ?is_cp(C6) ->
    Cs;
cdata_to_chars([C1,C2,C3,C4,C5,C6|Cs]) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4),
        ?is_cp(C5), ?is_cp(C6) ->
    [C1,C2,C3,C4,C5,C6 | cdata_to_chars(Cs)];
cdata_to_chars([C1,C2,C3,C4,C5]=Cs) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4),
        ?is_cp(C5) ->
    Cs;
cdata_to_chars([C1,C2,C3,C4,C5|Cs]) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4),
        ?is_cp(C5) ->
    [C1,C2,C3,C4,C5 | cdata_to_chars(Cs)];
cdata_to_chars([C1,C2,C3,C4]=Cs) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4) ->
    Cs;
cdata_to_chars([C1,C2,C3,C4|Cs]) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3), ?is_cp(C4) ->
    [C1,C2,C3,C4 | cdata_to_chars(Cs)];
cdata_to_chars([C1,C2,C3]=Cs) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3) ->
    Cs;
cdata_to_chars([C1,C2,C3|Cs]) when
        ?is_cp(C1), ?is_cp(C2), ?is_cp(C3) ->
    [C1,C2,C3 | cdata_to_chars(Cs)];
cdata_to_chars([C1,C2]=Cs) when
        ?is_cp(C1), ?is_cp(C2) ->
    Cs;
cdata_to_chars([C1,C2|Cs]) when
        ?is_cp(C1), ?is_cp(C2) ->
    [C1,C2 | cdata_to_chars(Cs)];
cdata_to_chars([C]=Cs) when ?is_cp(C) ->
    Cs;
cdata_to_chars([C|Cs]) when ?is_cp(C) ->
    [C | cdata_to_chars(Cs)];
cdata_to_chars([I|Cs]) ->
    [cdata_to_chars(I) | cdata_to_chars(Cs)].

limit_cdata_to_chars(Cs, 0, normal) ->
    L = limit_cdata_to_chars(Cs, 4, final),
    case string:length(L) of
        N when N < 4 -> L;
        4 -> "..."
    end;
limit_cdata_to_chars(_Cs, 0, final) -> [];
limit_cdata_to_chars(Cs, Limit, Mode) ->
    case string:next_grapheme(Cs) of
        {error, <<C,Cs1/binary>>} ->
            %% This is how ~ts handles Latin1 binaries with option
            %% chars_limit.
            [C | limit_cdata_to_chars(Cs1, Limit - 1, Mode)];
        {error, [C|Cs1]} -> % not all versions of module string return this
            [C | limit_cdata_to_chars(Cs1, Limit - 1, Mode)];
        []=Nil ->
            Nil;
        [GC|Cs1] ->
            [GC | limit_cdata_to_chars(Cs1, Limit - 1, Mode)]
    end.

limit_field(F, CharsLimit) when CharsLimit < 0; F =:= none ->
    F;
limit_field(F, CharsLimit) ->
    max(3, min(F, CharsLimit)).

%% string(String, Field, Adjust, Precision, PadChar)

string(S, none, _Adj, none, _Pad, _Enc) -> S;
string(S, F, Adj, none, Pad, Enc) ->
    string_field(S, F, Adj, io_lib:chars_length(S), Pad, Enc);
string(S, none, _Adj, P, Pad, Enc) ->
    string_field(S, P, left, io_lib:chars_length(S), Pad, Enc);
string(S, F, Adj, P, Pad, Enc) when F >= P ->
    N = io_lib:chars_length(S),
    if F > P ->
	    if N > P ->
		    adjust(flat_trunc(S, P, Enc), chars(Pad, F-P), Adj);
	       N < P ->
		    adjust([S|chars(Pad, P-N)], chars(Pad, F-P), Adj);
	       true -> % N == P
		    adjust(S, chars(Pad, F-P), Adj)
	    end;
       true -> % F == P
	    string_field(S, F, Adj, N, Pad, Enc)
    end.

string_field(S, F, _Adj, N, _Pad, Enc) when N > F ->
    flat_trunc(S, F, Enc);
string_field(S, F, Adj, N, Pad, _Enc) when N < F ->
    adjust(S, chars(Pad, F-N), Adj);
string_field(S, _, _, _, _, _) -> % N == F
    S.

%% unprefixed_integer(Int, Field, Adjust, Base, PadChar, Lowercase)
%% -> [Char].

unprefixed_integer(Int, F, Adj, Base, Pad, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
        Num =
            case Base of
                10 -> int_to_str(-Int);
                _ -> erlang:integer_to_list(-Int, Base)
            end,
	    S = cond_lowercase(Num, Base, Lowercase),
	    term([$-|S], F, Adj, none, Pad);
       true ->
        Num =
            case Base of
                10 -> int_to_str(Int);
                _ -> erlang:integer_to_list(Int, Base)
            end,
	    S = cond_lowercase(Num, Base, Lowercase),
	    term(S, F, Adj, none, Pad)
    end.

%% prefixed_integer(Int, Field, Adjust, Base, PadChar, Prefix, Lowercase)
%% -> [Char].

prefixed_integer(Int, F, Adj, Base, Pad, Prefix, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
        Num =
            case Base of
                10 -> int_to_str(-Int);
                _ -> erlang:integer_to_list(-Int, Base)
            end,
	    S = cond_lowercase(Num, Base, Lowercase),
	    term([$-,Prefix|S], F, Adj, none, Pad);
       true ->
        Num =
            case Base of
                10 -> int_to_str(Int);
                _ -> erlang:integer_to_list(Int, Base)
            end,
	    S = cond_lowercase(Num, Base, Lowercase),
	    term([Prefix|S], F, Adj, none, Pad)
    end.

%% char(Char, Field, Adjust, Precision, PadChar) -> chars().

char(C, none, _Adj, none, _Pad) -> [C];
char(C, F, _Adj, none, _Pad) -> chars(C, F);
char(C, none, _Adj, P, _Pad) -> chars(C, P);
char(C, F, Adj, P, Pad) when F >= P ->
    adjust(chars(C, P), chars(Pad, F - P), Adj).

%% newline(Field, Adjust, Precision, PadChar) -> [Char].

newline(none, _Adj, _P, _Pad) -> "\n";
newline(F, right, _P, _Pad) -> chars($\n, F).

%%
%% Utilities
%%

adjust(Data, [], _) -> Data;
adjust(Data, Pad, left) -> [Data|Pad];
adjust(Data, Pad, right) -> [Pad|Data].

%% Flatten and truncate a deep list to at most N elements.

flat_trunc(List, N, latin1) when is_integer(N), N >= 0 ->
    lists:sublist(lists:flatten(List), N); % TODO Only flatten up to N elements
flat_trunc(List, N, unicode) when is_integer(N), N >= 0 ->
    string:slice(List, 0, N).

%% A deep version of lists:duplicate/2
chars(_C, 0) ->
    [];
chars($\s, 1) -> % Optimise common cases
    [$\s];
chars($\s, 2) ->
    [$\s, $\s];
chars($\s, 3) ->
    [$\s, $\s, $\s];
chars($\s, 4) ->
    [$\s,$\s,$\s,$\s];
chars($\s, 5) ->
    [$\s,$\s,$\s,$\s,$\s];
chars($\s, 6) ->
    [$\s,$\s,$\s,$\s,$\s,$\s];
chars($\s, 7) ->
    [$\s,$\s,$\s,$\s,$\s,$\s,$\s];
chars($\s, 8) ->
    [$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s];
chars($\s, 9) ->
    [$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s];
chars($\s, 10) ->
    [$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s];
chars($\s, 11) ->
    [$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s];
chars($\s, 12) ->
    [$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s,$\s];
chars($\n, 1) ->
    [$\n];
chars($\n, 2) ->
    [$\n, $\n];
chars($\n, 3) ->
    [$\n, $\n, $\n];
chars($\n, 4) ->
    [$\n,$\n,$\n,$\n];
chars(C, 1) ->
    [C];
chars(C, 2) ->
    [C,C];
chars(C, 3) ->
    [C,C,C];
chars(C, 4) ->
    [C,C,C,C];
chars(C, 5) ->
    [C,C,C,C,C];
chars(C, 6) ->
    [C,C,C,C,C,C];
chars(C, 7) ->
    [C,C,C,C,C,C,C];
chars(C, 8) ->
    [C,C,C,C,C,C,C,C];
chars(C, N) when is_integer(N), (N band 1) =:= 0 ->
    S = chars(C, N bsr 1),
    [S|S];
chars(C, N) when is_integer(N) ->
    S = chars(C, N bsr 1),
    [C,S|S].

%chars(C, N, Tail) ->
%    [chars(C, N)|Tail].

%% Lowercase conversion

% Bases below 10 generate characters in the range 0-10, and over this range,
% lowercasing is the identity function, so we avoid allocating a copy of the
% string and return the original
cond_lowercase(String, Base, true) when Base > 10 ->
    lowercase(String);
cond_lowercase(String, _Base, _Lowercase) ->
    String.

lowercase([H|T]) when is_integer(H), H >= $A, H =< $Z ->
    [(H-$A+$a)|lowercase(T)];
lowercase([H|T]) ->
    [H|lowercase(T)];
lowercase([]=Nil) ->
    Nil.

%% Make sure T does change sign.
sub(T, _) when T < 0 -> T;
sub(T, E) when T >= E -> T - E;
sub(_, _) -> 0.

get_option(Key, TupleList, Default) ->
    case lists:keyfind(Key, 1, TupleList) of
	false -> Default;
	{Key, Value} -> Value;
	_ -> Default
    end.

prepend_duplicates(N, X, Tl) when is_integer(N), N >= 0 ->
    prepend_duplicates_1(N, X, Tl).

prepend_duplicates_1(0, _, Tl) ->
    Tl;
prepend_duplicates_1(1, X, Tl) ->
    [X|Tl];
prepend_duplicates_1(2, X, Tl) ->
    [X, X | Tl];
prepend_duplicates_1(3, X, Tl) ->
    [X, X, X | Tl];
prepend_duplicates_1(4, X, Tl) ->
    [X, X, X, X | Tl];
prepend_duplicates_1(5, X, Tl) ->
    [X, X, X, X, X | Tl];
prepend_duplicates_1(6, X, Tl) ->
    [X, X, X, X, X, X | Tl];
prepend_duplicates_1(7, X, Tl) ->
    [X, X, X, X, X, X, X | Tl];
prepend_duplicates_1(8, X, Tl) ->
    [X, X, X, X, X, X, X, X | Tl];
prepend_duplicates_1(N, X, Tl) -> % We know N > 8
    [X, X, X, X, X, X, X, X | prepend_duplicates_1(N - 8, X, Tl)].

% flatten_rev_chars(L,Acc) is equivalent to lists:append(lists:reverse(L) ++ Acc),
% but avoids the intermediate list
append_rev_chars([],Acc) ->
    Acc;
append_rev_chars([L1,L2,L3,L4],Acc) when is_list(L1), is_list(L2), is_list(L3), is_list(L4) ->
    L4++L3++L2++L1++Acc;
append_rev_chars([L1,L2,L3,L4|Tl],Acc) when is_list(L1), is_list(L2), is_list(L3), is_list(L4) ->
    append_rev_chars(Tl,L4++L3++L2++L1++Acc);
append_rev_chars([L1,L2,L3],Acc) when is_list(L1), is_list(L2), is_list(L3) ->
    L3++L2++L1++Acc;
append_rev_chars([L1,L2],Acc) when is_list(L1), is_list(L2) ->
    L2++L1++Acc;
append_rev_chars([L],Acc) when is_list(L) ->
    L++Acc.

append_rev_chars([L|Tl]) when is_list(L) ->
    append_rev_chars(Tl,L).

int_to_str(-1) -> "-1";
int_to_str(0) -> "0";
int_to_str(1) -> "1";
int_to_str(2) -> "2";
int_to_str(3) -> "3";
int_to_str(4) -> "4";
int_to_str(5) -> "5";
int_to_str(6) -> "6";
int_to_str(7) -> "7";
int_to_str(8) -> "8";
int_to_str(9) -> "9";
int_to_str(10) -> "10";
int_to_str(11) -> "11";
int_to_str(12) -> "12";
int_to_str(13) -> "13";
int_to_str(14) -> "14";
int_to_str(15) -> "15";
int_to_str(16) -> "16";
int_to_str(17) -> "17";
int_to_str(18) -> "18";
int_to_str(19) -> "19";
int_to_str(20) -> "20";
int_to_str(21) -> "21";
int_to_str(22) -> "22";
int_to_str(23) -> "23";
int_to_str(24) -> "24";
int_to_str(25) -> "25";
int_to_str(26) -> "26";
int_to_str(27) -> "27";
int_to_str(28) -> "28";
int_to_str(29) -> "29";
int_to_str(30) -> "30";
int_to_str(31) -> "31";
int_to_str(32) -> "32";
int_to_str(33) -> "33";
int_to_str(34) -> "34";
int_to_str(35) -> "35";
int_to_str(36) -> "36";
int_to_str(37) -> "37";
int_to_str(38) -> "38";
int_to_str(39) -> "39";
int_to_str(40) -> "40";
int_to_str(41) -> "41";
int_to_str(42) -> "42";
int_to_str(43) -> "43";
int_to_str(44) -> "44";
int_to_str(45) -> "45";
int_to_str(46) -> "46";
int_to_str(47) -> "47";
int_to_str(48) -> "48";
int_to_str(49) -> "49";
int_to_str(50) -> "50";
int_to_str(51) -> "51";
int_to_str(52) -> "52";
int_to_str(53) -> "53";
int_to_str(54) -> "54";
int_to_str(55) -> "55";
int_to_str(56) -> "56";
int_to_str(57) -> "57";
int_to_str(58) -> "58";
int_to_str(59) -> "59";
int_to_str(60) -> "60";
int_to_str(61) -> "61";
int_to_str(62) -> "62";
int_to_str(63) -> "63";
int_to_str(64) -> "64";
int_to_str(65) -> "65";
int_to_str(66) -> "66";
int_to_str(67) -> "67";
int_to_str(68) -> "68";
int_to_str(69) -> "69";
int_to_str(70) -> "70";
int_to_str(71) -> "71";
int_to_str(72) -> "72";
int_to_str(73) -> "73";
int_to_str(74) -> "74";
int_to_str(75) -> "75";
int_to_str(76) -> "76";
int_to_str(77) -> "77";
int_to_str(78) -> "78";
int_to_str(79) -> "79";
int_to_str(80) -> "80";
int_to_str(81) -> "81";
int_to_str(82) -> "82";
int_to_str(83) -> "83";
int_to_str(84) -> "84";
int_to_str(85) -> "85";
int_to_str(86) -> "86";
int_to_str(87) -> "87";
int_to_str(88) -> "88";
int_to_str(89) -> "89";
int_to_str(90) -> "90";
int_to_str(91) -> "91";
int_to_str(92) -> "92";
int_to_str(93) -> "93";
int_to_str(94) -> "94";
int_to_str(95) -> "95";
int_to_str(96) -> "96";
int_to_str(97) -> "97";
int_to_str(98) -> "98";
int_to_str(99) -> "99";
int_to_str(I) -> erlang:integer_to_list(I).
