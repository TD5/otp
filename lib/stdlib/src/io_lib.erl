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

%% This module is a library of useful i/o functions. It is hoped that the
%% functions defined in it are basic enough to be used without modification
%% as components of more complex utilities.
%%
%% It is completely self-contained and uses no other modules. Its own
%% utilities are exported.
%%
%% Most of the code here is derived from the original prolog versions and
%% from similar code written by Joe Armstrong and myself.
%%
%% This module has been split into separate modules:
%% io_lib        - basic write and utilities
%% io_lib_format - formatted output
%% io_lib_fread  - formatted input
%% io_lib_pretty - term prettyprinter

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037	NUL - US	control
%% 040 - 057	SPC - /		punctuation
%% 060 - 071	0 - 9		digit
%% 072 - 100	: - @		punctuation
%% 101 - 132	A - Z		uppercase
%% 133 - 140	[ - `		punctuation
%% 141 - 172	a - z		lowercase
%% 173 - 176	{ - ~		punctuation
%% 177		DEL		control
%% 200 - 237			control
%% 240 - 277	NBSP - ¿	punctuation
%% 300 - 326	À - Ö		uppercase
%% 327		×		punctuation
%% 330 - 336	Ø - Þ		uppercase
%% 337 - 366	ß - ö		lowercase
%% 367		÷		punctuation
%% 370 - 377	ø - ÿ		lowercase
%%
%% Many punctuation characters region have special meaning.  Must
%% watch using × \327, very close to x \170

-module(io_lib).
-moduledoc """
I/O library functions.

This module contains functions for converting to and from strings (lists of
characters). They are used for implementing the functions in the `m:io` module.
There is no guarantee that the character lists returned from some of the
functions are flat, they can be deep lists. Function `lists:flatten/1` can be
used for flattening deep lists.
""".

-export([fwrite/2,fwrite/3,fread/2,fread/3,format/2,format/3]).
-export([scan_format/2,unscan_format/1,build_text/1,build_text/2]).
-export([print/1,print/4,indentation/2]).

-export([write/1,write/2,write/3,nl/0,format_prompt/1,format_prompt/2]).
-export([write_acc/3]).
-export([write_binary/3]).
-export([write_atom/1,write_string/1,write_string/2,write_latin1_string/1,
         write_latin1_string/2, write_char/1, write_latin1_char/1]).

-export([write_atom_as_latin1/1, write_string_as_latin1/1,
         write_string_as_latin1/2, write_char_as_latin1/1]).

-export([quote_atom/2, char_list/1, latin1_char_list/1,
	 deep_char_list/1, deep_latin1_char_list/1,
	 printable_list/1, printable_latin1_list/1, printable_unicode_list/1]).

%% Utilities for collecting characters.
-export([collect_chars/3, collect_chars/4,
	 collect_line/3, collect_line/4,
	 get_until/3, get_until/4]).

%% The following functions were used by Yecc's include-file.
-export([write_unicode_string/1, write_unicode_char/1,
         deep_unicode_char_list/1]).

-export([limit_term/2]).

-export([chars_length/1]).

-export_type([chars/0, latin1_string/0, continuation/0,
              fread_error/0, fread_item/0, format_spec/0, chars_limit/0]).

%%----------------------------------------------------------------------

-doc "An possibly deep list containing only `t:char/0`s.".
-type chars() :: [char() | chars()].
-type latin1_string() :: [unicode:latin1_char()].
-type depth() :: -1 | non_neg_integer().

-doc "A continuation as returned by `fread/3`.".
-opaque continuation() :: {Format :: string(),
                           Stack :: chars(),
                           Nchars :: non_neg_integer(),
                           Results :: [term()]}.

-type fread_error() :: 'atom'
                     | 'based'
                     | 'character'
                     | 'float'
                     | 'format'
                     | 'input'
                     | 'integer'
                     | 'string'
                     | 'unsigned'.

-type fread_item() :: string() | atom() | integer() | float().

-doc """
A map describing the contents of a format string.

- `control_char` is the type of control sequence: `$P`, `$w`, and so on.
- `args` is a list of the arguments used by the control sequence, or an empty
  list if the control sequence does not take any arguments.
- `width` is the field width.
- `adjust` is the adjustment.
- `precision` is the precision of the printed argument.
- `pad_char` is the padding character.
- `encoding` is set to `true` if translation modifier `t` is present.
- `strings` is set to `false` if modifier `l` is present.
- `maps_order` is set to `undefined` by default, `ordered` if modifier `k` is
  present, or `reversed` or `CmpFun` if modifier `K` is present.
""".
-type format_spec() ::
        #{
           control_char := char(),
           args         := [any()],
           width        := 'none' | integer(),
           adjust       := 'left' | 'right',
           precision    := 'none' | integer(),
           pad_char     := char(),
           encoding     := 'unicode' | 'latin1',
           strings      := boolean(),
           % `maps_order` has been added since OTP26 and is optional
           maps_order   => maps:iterator_order()
         }.

%%----------------------------------------------------------------------

%% Interface calls to sub-modules.

-doc """
Returns a character list that represents `Data` formatted in accordance with
`Format`.

For a detailed description of the available formatting options, see
[`io:fwrite/1,2,3`](`io:fwrite/1`). If the format string or argument list
contains an error, a fault is generated.

If and only if the Unicode translation modifier is used in the format string
(that is, `~ts` or `~tc`), the resulting list can contain characters beyond the
ISO Latin-1 character range (that is, numbers > 255). If so, the result is still
an ordinary Erlang `t:string/0`, and can well be used in any context where
Unicode data is allowed.
""".
-spec fwrite(Format, Data) -> chars() when
      Format :: io:format(),
      Data :: [term()].

fwrite(Format, Args) ->
    format(Format, Args).

-type chars_limit() :: integer().

-doc """
Returns a character list that represents `Data` formatted in accordance with
`Format` in the same way as `fwrite/2` and `format/2`, but takes an extra
argument, a list of options.

Valid option:

- **`{chars_limit, CharsLimit}`** - A soft limit on the number of characters
  returned. When the number of characters is reached, remaining structures are
  replaced by "`...`". `CharsLimit` defaults to -1, which means no limit on the
  number of characters returned.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec fwrite(Format, Data, Options) -> chars() when
      Format :: io:format(),
      Data :: [term()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: chars_limit().

fwrite(Format, Args, Options) ->
    format(Format, Args, Options).

-doc """
Tries to read `String` in accordance with the control sequences in `Format`.

For a detailed description of the available formatting options, see `io:fread/3`.
It is assumed that `String` contains whole lines.

The function returns:

- **`{ok, InputList, LeftOverChars}`** - The string was read. `InputList` is the
  list of successfully matched and read items, and `LeftOverChars` are the input
  characters not used.

- **`{more, RestFormat, Nchars, InputStack}`** - The string was read, but more
  input is needed to complete the original format string. `RestFormat` is the
  remaining format string, `Nchars` is the number of characters scanned, and
  `InputStack` is the reversed list of inputs matched up to that point.

- **`{error, What}`** - The read operation failed and parameter `What` gives a
  hint about the error.

_Example:_

```erlang
3> io_lib:fread("~f~f~f", "15.6 17.3e-6 24.5").
{ok,[15.6,1.73e-5,24.5],[]}
```
""".
-spec fread(Format, String) -> Result when
      Format :: string(),
      String :: string(),
      Result :: {'ok', InputList :: [fread_item()], LeftOverChars :: string()}
              | {'more', RestFormat :: string(),
                 Nchars :: non_neg_integer(),
                 InputStack :: chars()}
              | {'error', {'fread', What :: fread_error()}}.

fread(Chars, Format) ->
    io_lib_fread:fread(Chars, Format).

-doc """
This is the re-entrant formatted reader. The continuation of the first call to
the functions must be `[]`.

For a complete description of how the re-entrant input scheme works,
see Armstrong, Virding, Williams: 'Concurrent Programming in
Erlang', Chapter 13.

The function returns:

- **`{done, Result, LeftOverChars}`** - The input is complete. The result is one
  of the following:

  - **`{ok, InputList}`** - The string was read. `InputList` is the list of
    successfully matched and read items, and `LeftOverChars` are the remaining
    characters.

  - **`eof`** - End of file was encountered. `LeftOverChars` are the input
    characters not used.

  - **`{error, What}`** - An error occurred and parameter `What` gives a hint
    about the error.

- **`{more, Continuation}`** - More data is required to build a term.
  `Continuation` must be passed to [`fread/3`](`fread/3`) when more data becomes
  available.
""".
-spec fread(Continuation, CharSpec, Format) -> Return when
      Continuation :: continuation() | [],
      CharSpec :: string() | 'eof',
      Format :: string(),
      Return :: {'more', Continuation1 :: continuation()}
              | {'done', Result, LeftOverChars :: string()},
      Result :: {'ok', InputList :: [fread_item()]}
              | 'eof'
              | {'error', {'fread', What :: fread_error()}}.

fread(Cont, Chars, Format) ->
    io_lib_fread:fread(Cont, Chars, Format).

-doc(#{equiv => fwrite(Format, Data)}).
-spec format(Format, Data) -> chars() when
      Format :: io:format(),
      Data :: [term()].

format(Format, Args) ->
    try io_lib_format:fwrite(Format, Args)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [Format, Args])
    end.

-doc(#{equiv => fwrite(Format, Data, Options)}).
-doc(#{since => <<"OTP 21.0">>}).
-spec format(Format, Data, Options) -> chars() when
      Format :: io:format(),
      Data :: [term()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: chars_limit().

format(Format, Args, Options) ->
    try io_lib_format:fwrite(Format, Args, Options)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [Format, Args])
    end.

-doc """
Returns a list corresponding to the specified format string, where control
sequences have been replaced with corresponding tuples. This list can be passed
to:

- `build_text/1` to have the same effect as [`format(Format, Args)`](`format/2`)
- `unscan_format/1` to get the corresponding pair of `Format` and `Args` (with
  every `*` and corresponding argument expanded to numeric values)

A typical use of this function is to replace unbounded-size control sequences
like `~w` and `~p` with the depth-limited variants `~W` and `~P` before
formatting to text in, for example, a logger.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec scan_format(Format, Data) -> FormatList when
      Format :: io:format(),
      Data :: [term()],
      FormatList :: [char() | format_spec()].

scan_format(Format, Args) ->
    try io_lib_format:scan(Format, Args)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [Format, Args])
    end.

-doc "For details, see `scan_format/2`.".
-doc(#{since => <<"OTP 18.0">>}).
-spec unscan_format(FormatList) -> {Format, Data} when
      FormatList :: [char() | format_spec()],
      Format :: io:format(),
      Data :: [term()].

unscan_format(FormatList) ->
    io_lib_format:unscan(FormatList).

-doc "For details, see `scan_format/2`.".
-doc(#{since => <<"OTP 18.0">>}).
-spec build_text(FormatList) -> chars() when
      FormatList :: [char() | format_spec()].

build_text(FormatList) ->
    try io_lib_format:build(FormatList)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [FormatList])
    end.

-doc false.
-spec build_text(FormatList, Options) -> chars() when
      FormatList :: [char() | format_spec()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: chars_limit().

build_text(FormatList, Options) ->
    try io_lib_format:build(FormatList, Options)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [FormatList, Options])
    end.

%% Failure to load a module must not be labeled as badarg.
%% C, R, and S are included so that the original error, which could be
%% a bug in io_lib_format, can be found by tracing on
%% test_modules_loaded/3.
test_modules_loaded(_C, _R, _S) ->
    Modules = [io_lib_format, io_lib_pretty, string, unicode],
    case code:ensure_modules_loaded(Modules) of
        ok -> ok;
        Error -> erlang:error(Error)
    end.

-doc(#{equiv => print(Term, 1, 80, -1)}).
-spec print(Term) -> chars() when
      Term :: term().

print(Term) ->
    io_lib_pretty:print(Term).

-doc """
Returns a list of characters that represents `Term`, but breaks representations
longer than one line into many lines and indents each line sensibly.

Also tries to detect and output lists of printable characters as strings.

- `Column` is the starting column; defaults to 1.
- `LineLength` is the maximum line length; defaults to 80.
- `Depth` is the maximum print depth; defaults to -1, which means no limitation.
""".
-spec print(Term, Column, LineLength, Depth) -> chars() when
      Term :: term(),
      Column :: non_neg_integer(),
      LineLength :: non_neg_integer(),
      Depth :: depth().

print(Term, Column, LineLength, Depth) ->
    io_lib_pretty:print(Term, Column, LineLength, Depth).

-doc "Returns the indentation if `String` has been printed, starting at `StartIndent`.".
-spec indentation(String, StartIndent) -> integer() when
      String :: string(),
      StartIndent :: integer().

indentation(Chars, Current) ->
    io_lib_format:indentation(Chars, Current).


%% Format an IO-request prompt (handles formatting errors safely).
%% Atoms, binaries, and iolists (or unicode:charlist()) can be used
%% as-is, and will be printed without any additional quotes.

-doc false.
-spec format_prompt(term()) -> chars().

format_prompt(Prompt) ->
    format_prompt(Prompt, latin1).

-doc false.
-spec format_prompt(term(), atom()) -> chars().

format_prompt({format,Format,Args}, _Encoding) ->
    do_format_prompt(Format, Args);
format_prompt(Prompt, Encoding)
  when is_list(Prompt); is_atom(Prompt); is_binary(Prompt) ->
    do_format_prompt(prompt_format_str(Encoding), [Prompt]);
format_prompt(Prompt, Encoding) ->
    do_format_prompt(prompt_format(Encoding), [Prompt]).

do_format_prompt(Format, Args) ->
    case catch format(Format, Args) of
	{'EXIT',_} -> "???";
	List -> List
    end.

prompt_format_str(latin1) ->
    "~s";
prompt_format_str(_) ->
    "~ts".

prompt_format(latin1) ->
    "~p";
prompt_format(_) ->
    "~tp".

%% write(Term)
%% write(Term, Depth)
%% write(Term, Depth, Pretty)
%%  Return a (non-flattened) list of characters giving a printed
%%  representation of the term. write/3 is for backward compatibility.

-doc(#{equiv => write(Term, -1)}).
-spec write(Term) -> chars() when
      Term :: term().

write(Term) ->
    write1(Term, -1, latin1, undefined).

-doc false.
-spec write(term(), depth(), boolean()) -> chars().

write(Term, D, true) ->
    io_lib_pretty:print(Term, 1, 80, D);
write(Term, D, false) ->
    write(Term, D).

-doc """
write(Term, DepthOrOptions)

Returns a character list that represents `Term`. Option `Depth` controls the
depth of the structures written.

When the specified depth is reached, everything below this level is replaced by
"`...`".

`Depth` defaults to -1, which means no limitation. Option `CharsLimit` puts a
soft limit on the number of characters returned. When the number of characters is
reached, remaining structures are replaced by "`...`". `CharsLimit` defaults to -1,
which means no limit on the number of characters returned.

_Example:_

```erlang
1> lists:flatten(io_lib:write({1,[2],[3],[4,5],6,7,8,9})).
"{1,[2],[3],[4,5],6,7,8,9}"
2> lists:flatten(io_lib:write({1,[2],[3],[4,5],6,7,8,9}, 5)).
"{1,[2],[3],[...],...}"
3> lists:flatten(io_lib:write({[1,2,3],[4,5],6,7,8,9}, [{chars_limit,20}])).
"{[1,2|...],[4|...],...}"
```
""".
-spec write(Term, Depth) -> chars() when
      Term :: term(),
      Depth :: depth();
           (Term, Options) -> chars() when
      Term :: term(),
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit}
              | {'depth', Depth}
              | {'encoding', 'latin1' | 'utf8' | 'unicode'},
      CharsLimit :: chars_limit(),
      Depth :: depth().

write(Term, DepthOrOptions) ->
    write_acc(Term, DepthOrOptions, "").

% Hook for making tweaks to general prepending logic, e.g.
% by inspecting the length of the two strings to decide
% whether to flatten them immediately
-define(prepend(Str,Tl), [Str|Tl]).

-doc false.
write_acc(Term, Options, Tl) when is_list(Options) ->
    Depth = get_option(depth, Options, -1),
    Encoding = get_option(encoding, Options, epp:default_encoding()),
    CharsLimit = get_option(chars_limit, Options, -1),
    MapsOrder = get_option(maps_order, Options, undefined),
    if
        Depth =:= 0; CharsLimit =:= 0 ->
            ?prepend("...",Tl);
        is_integer(CharsLimit), CharsLimit < 0, is_integer(Depth) ->
            case Tl of
                [] -> write1(Term, Depth, Encoding, MapsOrder);
                _ -> write1(Term, Depth, Encoding, MapsOrder, Tl)
            end;
        is_integer(CharsLimit), CharsLimit > 0 ->
            RecDefFun = fun(_, _) -> no end,
            If = io_lib_pretty:intermediate
                 (Term, Depth, CharsLimit, RecDefFun, Encoding, _Str=false, MapsOrder),
            ?prepend(io_lib_pretty:write(If),Tl)
    end;
write_acc(Term, Depth, Tl) ->
    write_acc(Term, [{depth, Depth}, {encoding, latin1}], Tl).

write1(_Term, 0, _E, _O) ->
    "...";
write1([], _D, _E, _O) ->
    "[]";
write1({}, _D, _E, _O) ->
    "{}";
write1([H], D, E, O) ->
    case D of
        1 -> "[...]";
        _ when D < 0 ->
            [$[|write1(H, -1, E, O, "]")];
        _ ->
            [$[|write1(H, D-1, E, O, "]")]
    end;
write1([H|T], D, E, O) ->
    case D of
        1 -> "[...]";
        _ when D < 0 ->
            [$[|write1(H, -1, E, O, write_tail_unlimited_depth(T, E, O, []))];
        _ ->
            [$[|write1(H, D-1, E, O, write_tail(T, D-1, E, O, []))]
    end;
write1(#{}=Term, D, E, O) ->
    if
        D < 0 ->
            write_map_unlimited_depth(Term, E, O, []);
        true ->
            write_map(Term, D, E, O, [])
    end;
write1(Term, _D, _E, _O) when is_integer(Term) -> int_to_str(Term);
write1(Term, _D, _E, _O) when is_float(Term) -> io_lib_format:fwrite_g(Term);
write1(Atom, _D, latin1, _O) when is_atom(Atom) -> write_atom_as_latin1(Atom);
write1(Atom, _D, _E, _O) when is_atom(Atom) -> write_atom(Atom);
write1(T, D, E, O) when is_tuple(T) ->
    case D of
        1 -> "{...}";
        _ when D < 0 -> write_tuple_unlimited_depth(T, E, O, []);
        _ -> write_tuple(T, D, E, O, [])
    end;
write1(Term, D, _E, _O) when is_bitstring(Term) ->
    write_binary_acc(Term, D, []);
write1(Term, _D, _E, _O) when is_port(Term) -> write_port(Term);
write1(Term, _D, _E, _O) when is_pid(Term) -> pid_to_list(Term);
write1(Term, _D, _E, _O) when is_reference(Term) -> write_ref(Term);
write1(F, _D, _E, _O) when is_function(F) ->
    erlang:fun_to_list(F).

write1(_Term, 0, _E, _O, Tl) ->
    ?prepend("...",Tl);
write1([], _D, _E, _O, Tl) ->
    [$[,$]|Tl];
write1({}, _D, _E, _O, Tl) ->
    [${,$}|Tl];
write1([H], D, E, O, Tl) ->
    case D of
        1 -> ?prepend("[...]",Tl);
        _ when D < 0 ->
            [$[|write1(H, -1, E, O, [$]|Tl])];
        _ ->
            [$[|write1(H, D-1, E, O, [$]|Tl])]
    end;
write1([H|T], D, E, O, Tl) ->
    case D of
        1 -> ?prepend("[...]",Tl);
        _ when D < 0 ->
            [$[|write1(H, -1, E, O, write_tail_unlimited_depth(T, E, O, Tl))];
        _ ->
            [$[|write1(H, D-1, E, O, write_tail(T, D-1, E, O, Tl))]
    end;
write1(#{}=Term, D, E, O, Tl) ->
    if
        D < 0 ->
            write_map_unlimited_depth(Term, E, O, Tl);
        true ->
            write_map(Term, D, E, O, Tl)
    end;
write1(Term, _D, _E, _O, Tl) when is_integer(Term) -> ?prepend(int_to_str(Term), Tl);
write1(Term, _D, _E, _O, Tl) when is_float(Term) -> ?prepend(io_lib_format:fwrite_g(Term), Tl);
write1(Atom, _D, latin1, _O, Tl) when is_atom(Atom) -> write_atom_as_latin1_acc(Atom, Tl);
write1(Atom, _D, _E, _O, Tl) when is_atom(Atom) -> write_atom_acc(Atom, Tl);
write1(T, D, E, O, Tl) when is_tuple(T) ->
    case D of
        1 -> ?prepend("{...}",Tl);
        _ when D < 0 -> write_tuple_unlimited_depth(T, E, O, Tl);
        _ -> write_tuple(T, D, E, O, Tl)
    end;
write1(Term, D, _E, _O, Tl) when is_bitstring(Term) ->
    write_binary_acc(Term, D, Tl);
write1(Term, _D, _E, _O, Tl) when is_port(Term) -> ?prepend(write_port(Term), Tl);
write1(Term, _D, _E, _O, Tl) when is_pid(Term) -> ?prepend(pid_to_list(Term), Tl);
write1(Term, _D, _E, _O, Tl) when is_reference(Term) -> ?prepend(write_ref(Term), Tl);
write1(F, _D, _E, _O, Tl) when is_function(F) ->
    ?prepend(erlang:fun_to_list(F), Tl).

%% write_tail(List, Depth, Encoding)
%%  Test the terminating case first as this looks better with depth.

write_tail([], _D, _E, _O, Tl) -> [$]|Tl];
write_tail(_, 1, _E, _O, Tl) -> ?prepend("|...]",Tl);
write_tail([H], D, E, O, Tl) ->
    [$,|write1(H, D-1, E, O, [$]|Tl])];
write_tail([H1|_], 2, E, O, Tl) ->
    [$,|write1(H1, 1, E, O, ?prepend("|...]",Tl))];
write_tail([H1,H2], D, E, O, Tl) ->
    [$,|write1(H1, D-1, E, O, [$, | write1(H2, D-2, E, O, [$]|Tl])])];
write_tail([H1,H2|_], 3, E, O, Tl) ->
    [$,|write1(H1, 2, E, O, [$,|write1(H2, 1, E, O, ?prepend("|...]", Tl))])];
write_tail([H1,H2|T], D, E, O, Tl) ->
    [$,|write1(H1, D-1, E, O, [$,|write1(H2, D-2, E, O, write_tail(T, D-2, E, O, Tl))])];
write_tail(Other, D, E, O, Tl) ->
    [$||write1(Other, D-1, E, O, [$]|Tl])].

write_tail_unlimited_depth([], _E, _O, Tl) ->
    [$]|Tl];
write_tail_unlimited_depth([H], E, O, Tl) ->
    [$,|write1(H, -1, E, O, [$]|Tl])];
write_tail_unlimited_depth([H1,H2], E, O, Tl) ->
    UD=-1,
    [$,|write1(H1, UD, E, O, [$,|write1(H2, UD, E, O, [$]|Tl])])];
write_tail_unlimited_depth([H1,H2,H3], E, O, Tl) ->
    UD=-1,
    [$,|write1(H1, UD, E, O,
        [$,|write1(H2, UD, E, O,
        [$,|write1(H3, UD, E, O, [$]|Tl])])])];
write_tail_unlimited_depth([H1,H2,H3,H4], E, O, Tl) ->
    UD=-1,
    [$,|write1(H1, UD, E, O,
        [$,|write1(H2, UD, E, O,
        [$,|write1(H3, UD, E, O,
        [$,|write1(H4, UD, E, O, [$]|Tl])])])])];
write_tail_unlimited_depth([H1,H2,H3,H4,H5], E, O, Tl) ->
    UD=-1,
    [$,|write1(H1, UD, E, O,
        [$,|write1(H2, UD, E, O,
        [$,|write1(H3, UD, E, O,
        [$,|write1(H4, UD, E, O,
        [$,|write1(H5, UD, E, O, [$]|Tl])])])])])];
write_tail_unlimited_depth([H1,H2,H3,H4,H5,H6], E, O, Tl) ->
    UD=-1,
    [$,|write1(H1, UD, E, O,
        [$,|write1(H2, UD, E, O,
        [$,|write1(H3, UD, E, O,
        [$,|write1(H4, UD, E, O,
        [$,|write1(H5, UD, E, O,
        [$,|write1(H6, UD, E, O, [$]|Tl])])])])])])];
write_tail_unlimited_depth([H1,H2,H3,H4,H5,H6,H7], E, O, Tl) ->
    UD=-1,
    [$,|write1(H1, UD, E, O,
        [$,|write1(H2, UD, E, O,
        [$,|write1(H3, UD, E, O,
        [$,|write1(H4, UD, E, O,
        [$,|write1(H5, UD, E, O,
        [$,|write1(H6, UD, E, O,
        [$,|write1(H7, UD, E, O, [$]|Tl])])])])])])])];
write_tail_unlimited_depth([H1,H2,H3,H4,H5,H6,H7,H8], E, O, Tl) ->
    UD=-1,
    [$,|write1(H1, UD, E, O,
        [$,|write1(H2, UD, E, O,
        [$,|write1(H3, UD, E, O,
        [$,|write1(H4, UD, E, O,
        [$,|write1(H5, UD, E, O,
        [$,|write1(H6, UD, E, O,
        [$,|write1(H7, UD, E, O,
        [$,|write1(H8, UD, E, O, [$]|Tl])])])])])])])])];
write_tail_unlimited_depth([H1,H2,H3,H4,H5,H6,H7,H8|T], E, O, Tl) ->
    UD=-1,
    [$,|write1(H1, UD, E, O,
        [$,|write1(H2, UD, E, O,
        [$,|write1(H3, UD, E, O,
        [$,|write1(H4, UD, E, O,
        [$,|write1(H5, UD, E, O,
        [$,|write1(H6, UD, E, O,
        [$,|write1(H7, UD, E, O,
        [$,|write1(H8, UD, E, O, write_tail_unlimited_depth(T, E, O, Tl))])])])])])])])];
write_tail_unlimited_depth([H1|ImproperTail], E, O, Tl) ->
    [$,|write1(H1, 1, E, O, write_tail_unlimited_depth(ImproperTail, E, O, Tl))];
write_tail_unlimited_depth(ImproperTail, E, O, Tl) ->
    [$||write1(ImproperTail, -1, E, O, [$]|Tl])].

write_tuple_tail(T, I, _D, _E, _O, Tl) when I > tuple_size(T) ->
    [$}|Tl];
write_tuple_tail(_, _I, 1, _E, _O, Tl) ->
    ?prepend(",...}",Tl);
write_tuple_tail(T, I, D, E, O, Tl) ->
    [$,|write1(element(I, T), D-1, E, O, write_tuple_tail(T, I+1, D-1, E, O, Tl))].

write_tuple_tail_unlimited(T, I, _E, _O, Tl) when I > tuple_size(T) ->
    [$}|Tl];
write_tuple_tail_unlimited(T, I, E, O, Tl) ->
    [$,|write1(element(I, T), -1, E, O, write_tuple_tail_unlimited(T, I+1, E, O, Tl))].

% Check whether the limiting factor is the tuple size or the depth,
% so we can efficiently pattern match on the optimal parameter
write_tuple(T, D, E, O, Tl) when tuple_size(T) >= D ->
    write_partial_tuple(T, D, E, O, Tl);
write_tuple(T, D, E, O, Tl) ->
    write_entire_tuple(T, D, E, O, Tl).

write_entire_tuple({T1}, D, E, O, Tl) ->
    [${|write1(T1, D-1, E, O,[$}|Tl])];
write_entire_tuple({T1,T2}, D, E, O, Tl) ->
    [${|write1(T1, D-1, E, O,
        [$,|write1(T2, D-2, E, O,[$}|Tl])])];
write_entire_tuple({T1,T2,T3}, D, E, O, Tl) ->
    [${|write1(T1, D-1, E, O,
        [$,|write1(T2, D-2, E, O,
        [$,|write1(T3, D-3, E, O,[$}|Tl])])])];
write_entire_tuple({T1,T2,T3,T4}, D, E, O, Tl) ->
    [${|write1(T1, D-1, E, O,
        [$,|write1(T2, D-2, E, O,
        [$,|write1(T3, D-3, E, O,
        [$,|write1(T4, D-4, E, O,[$}|Tl])])])])];
write_entire_tuple({T1,T2,T3,T4,T5}, D, E, O, Tl) ->
    [${|write1(T1, D-1, E, O,
        [$,|write1(T2, D-2, E, O,
        [$,|write1(T3, D-3, E, O,
        [$,|write1(T4, D-4, E, O,
        [$,|write1(T5, D-5, E, O,[$}|Tl])])])])])];
write_entire_tuple({T1,T2,T3,T4,T5,T6}, D, E, O, Tl) ->
    [${|write1(T1, D-1, E, O,
        [$,|write1(T2, D-2, E, O,
        [$,|write1(T3, D-3, E, O,
        [$,|write1(T4, D-4, E, O,
        [$,|write1(T5, D-5, E, O,
        [$,|write1(T6, D-6, E, O,[$}|Tl])])])])])])];
write_entire_tuple({T1,T2,T3,T4,T5,T6,T7}, D, E, O, Tl) ->
    [${|write1(T1, D-1, E, O,
        [$,|write1(T2, D-2, E, O,
        [$,|write1(T3, D-3, E, O,
        [$,|write1(T4, D-4, E, O,
        [$,|write1(T5, D-5, E, O,
        [$,|write1(T6, D-6, E, O,
        [$,|write1(T7, D-7, E, O,[$}|Tl])])])])])])])];
write_entire_tuple({T1,T2,T3,T4,T5,T6,T7,T8}, D, E, O, Tl) ->
    [${|write1(T1, D-1, E, O,
        [$,|write1(T2, D-2, E, O,
        [$,|write1(T3, D-3, E, O,
        [$,|write1(T4, D-4, E, O,
        [$,|write1(T5, D-5, E, O,
        [$,|write1(T6, D-6, E, O,
        [$,|write1(T7, D-7, E, O,
        [$,|write1(T8, D-8, E, O, [$}|Tl])])])])])])])])];
write_entire_tuple(T, D, E, O, Tl) ->
    [${|write1(element(1,T), D-1, E, O,
        [$,|write1(element(2,T), D-2, E, O,
        [$,|write1(element(3,T), D-3, E, O,
        [$,|write1(element(4,T), D-4, E, O,
        [$,|write1(element(5,T), D-5, E, O,
        [$,|write1(element(6,T), D-6, E, O,
        [$,|write1(element(7,T), D-7, E, O,
        [$,|write1(element(8,T), D-8, E, O, write_tuple_tail(T, 9, D-8, E, O, Tl))])])])])])])])].

write_partial_tuple(T, 2, E, O, Tl) ->
    [${|write1(element(1,T), 1, E, O, ?prepend(",...}",Tl))];
write_partial_tuple(T, 3, E, O, Tl) ->
    [${|write1(element(1,T), 2, E, O,
        [$,|write1(element(2,T), 1, E, O, ?prepend(",...}", Tl))])];
write_partial_tuple(T, 4, E, O, Tl) ->
    [${|write1(element(1,T), 3, E, O,
        [$,|write1(element(2,T), 2, E, O,
        [$,|write1(element(3,T), 1, E, O, ?prepend(",...}", Tl))])])];
write_partial_tuple(T, 5, E, O, Tl) ->
    [${|write1(element(1,T), 4, E, O,
        [$,|write1(element(2,T), 3, E, O,
        [$,|write1(element(3,T), 2, E, O,
        [$,|write1(element(4,T), 1, E, O, ?prepend(",...}", Tl))])])])];
write_partial_tuple(T, 6, E, O, Tl) ->
    [${|write1(element(1,T), 5, E, O,
        [$,|write1(element(2,T), 4, E, O,
        [$,|write1(element(3,T), 3, E, O,
        [$,|write1(element(4,T), 2, E, O,
        [$,|write1(element(5,T), 1, E, O, ?prepend(",...}", Tl))])])])])];
write_partial_tuple(T, 7, E, O, Tl) ->
    [${|write1(element(1,T), 6, E, O,
        [$,|write1(element(2,T), 5, E, O,
        [$,|write1(element(3,T), 4, E, O,
        [$,|write1(element(4,T), 3, E, O,
        [$,|write1(element(5,T), 2, E, O,
        [$,|write1(element(6,T), 1, E, O, ?prepend(",...}", Tl))])])])])])];
write_partial_tuple(T, 8, E, O, Tl) ->
    [${|write1(element(1,T), 7, E, O,
        [$,|write1(element(2,T), 6, E, O,
        [$,|write1(element(3,T), 5, E, O,
        [$,|write1(element(4,T), 4, E, O,
        [$,|write1(element(5,T), 3, E, O,
        [$,|write1(element(6,T), 2, E, O,
        [$,|write1(element(7,T), 1, E, O, ?prepend(",...}", Tl))])])])])])])];
write_partial_tuple(T, D, E, O, Tl) ->
    [${|write1(element(1,T), D-1, E, O,
        [$,|write1(element(2,T), D-2, E, O,
        [$,|write1(element(3,T), D-3, E, O,
        [$,|write1(element(4,T), D-4, E, O,
        [$,|write1(element(5,T), D-5, E, O,
        [$,|write1(element(6,T), D-6, E, O,
        [$,|write1(element(7,T), D-7, E, O, write_tuple_tail(T, 8, D-7, E, O, Tl))])])])])])])].

write_tuple_unlimited_depth({T1}, E, O, Tl) ->
    UD=-1,
    [${|write1(T1, UD, E, O,[$}|Tl])];
write_tuple_unlimited_depth({T1,T2}, E, O, Tl) ->
    UD=-1,
    [${|write1(T1, UD, E, O,
        [$,|write1(T2, UD, E, O,[$}|Tl])])];
write_tuple_unlimited_depth({T1,T2,T3}, E, O, Tl) ->
    UD=-1,
    [${|write1(T1, UD, E, O,
        [$,|write1(T2, UD, E, O,
        [$,|write1(T3, UD, E, O,[$}|Tl])])])];
write_tuple_unlimited_depth({T1,T2,T3,T4}, E, O, Tl) ->
    UD=-1,
    [${|write1(T1, UD, E, O,
        [$,|write1(T2, UD, E, O,
        [$,|write1(T3, UD, E, O,
        [$,|write1(T4, UD, E, O,[$}|Tl])])])])];
write_tuple_unlimited_depth({T1,T2,T3,T4,T5}, E, O, Tl) ->
    UD=-1,
    [${|write1(T1, UD, E, O,
        [$,|write1(T2, UD, E, O,
        [$,|write1(T3, UD, E, O,
        [$,|write1(T4, UD, E, O,
        [$,|write1(T5, UD, E, O,[$}|Tl])])])])])];
write_tuple_unlimited_depth({T1,T2,T3,T4,T5,T6}, E, O, Tl) ->
    UD=-1,
    [${|write1(T1, UD, E, O,
        [$,|write1(T2, UD, E, O,
        [$,|write1(T3, UD, E, O,
        [$,|write1(T4, UD, E, O,
        [$,|write1(T5, UD, E, O,
        [$,|write1(T6, UD, E, O,[$}|Tl])])])])])])];
write_tuple_unlimited_depth({T1,T2,T3,T4,T5,T6,T7}, E, O, Tl) ->
    UD=-1,
    [${|write1(T1, UD, E, O,
        [$,|write1(T2, UD, E, O,
        [$,|write1(T3, UD, E, O,
        [$,|write1(T4, UD, E, O,
        [$,|write1(T5, UD, E, O,
        [$,|write1(T6, UD, E, O,
        [$,|write1(T7, UD, E, O,[$}|Tl])])])])])])])];
write_tuple_unlimited_depth({T1,T2,T3,T4,T5,T6,T7,T8}, E, O, Tl) ->
    UD=-1,
    [${|write1(T1, UD, E, O,
        [$,|write1(T2, UD, E, O,
        [$,|write1(T3, UD, E, O,
        [$,|write1(T4, UD, E, O,
        [$,|write1(T5, UD, E, O,
        [$,|write1(T6, UD, E, O,
        [$,|write1(T7, UD, E, O,
        [$,|write1(T8, UD, E, O, [$}|Tl])])])])])])])])];
write_tuple_unlimited_depth(T, E, O, Tl) ->
    UD=-1,
    [${|write1(element(1,T), UD, E, O,
        [$,|write1(element(2,T), UD, E, O,
        [$,|write1(element(3,T), UD, E, O,
        [$,|write1(element(4,T), UD, E, O,
        [$,|write1(element(5,T), UD, E, O,
        [$,|write1(element(6,T), UD, E, O,
        [$,|write1(element(7,T), UD, E, O,
        [$,|write1(element(8,T), UD, E, O, write_tuple_tail_unlimited(T, 9, E, O, Tl))])])])])])])])].

write_port(Port) ->
    erlang:port_to_list(Port).

write_ref(Ref) ->
    erlang:ref_to_list(Ref).

write_map(_, 1, _E, _O, Tl) -> ?prepend("#{}",Tl);
write_map(Map, _D, _E, _O, Tl) when map_size(Map) =:= 0 -> ?prepend("#{}",Tl);
write_map(Map, D, E, O, Tl) when is_integer(D) ->
    I = maps:iterator(Map, O),
    {K, V, NextI} = maps:next(I),
    D0 = D - 1,
    [$#,${ | write_map_assoc(K, V, D0, E, O, write_map_body(NextI, D0, D0, E, O, Tl))].

write_map_body(_, 1, _D0, _E, _O, Tl) -> ?prepend(",...}",Tl);
write_map_body(I, D, D0, E, O, Tl) ->
    case maps:next(I) of
        {K, V, NextI} ->
            [$, | write_map_assoc(K, V, D0, E, O, write_map_body(NextI, D - 1, D0, E, O, Tl))];
        none -> [$}|Tl]
    end.

write_map_unlimited_depth(Map, _E, _O, Tl) when map_size(Map) =:= 0 ->
    ?prepend("#{}",Tl);
% Faster, but produces an intermediary list proportional to the size of the
% map, so we limit to a conservative, finite size
write_map_unlimited_depth(Map, E, O=undefined, Tl) when map_size(Map) < 8 ->
    UD=-1,
    case maps:to_list(Map) of
      [{K1,V1}] ->
        [$#,${| write_map_assoc(K1, V1, UD, E, O, [$}|Tl])];
      [{K1,V1},{K2,V2}] ->
        [$#,${| write_map_assoc(K1, V1, UD, E, O,
          [$,|write_map_assoc(K2, V2, UD, E, O, [$}|Tl])])];
      [{K1,V1},{K2,V2},{K3,V3}] ->
        [$#,${| write_map_assoc(K1, V1, UD, E, O,
          [$,|write_map_assoc(K2, V2, UD, E, O,
          [$,|write_map_assoc(K3, V3, UD, E, O, [$]|Tl])])])];
      [{K1,V1},{K2,V2},{K3,V3},{K4,V4}] ->
        [$#,${| write_map_assoc(K1, V1, UD, E, O,
          [$,|write_map_assoc(K2, V2, UD, E, O,
          [$,|write_map_assoc(K3, V3, UD, E, O,
          [$,|write_map_assoc(K4, V4, UD, E, O, [$]|Tl])])])])];
      [{K1,V1},{K2,V2},{K3,V3},{K4,V4},{K5,V5}] ->
        [$#,${| write_map_assoc(K1, V1, UD, E, O,
          [$,|write_map_assoc(K2, V2, UD, E, O,
          [$,|write_map_assoc(K3, V3, UD, E, O,
          [$,|write_map_assoc(K4, V4, UD, E, O,
          [$,|write_map_assoc(K5, V5, UD, E, O, [$]|Tl])])])])])];
      [{K1,V1},{K2,V2},{K3,V3},{K4,V4},{K5,V5},{K6,V6}] ->
        [$#,${| write_map_assoc(K1, V1, UD, E, O,
          [$,|write_map_assoc(K2, V2, UD, E, O,
          [$,|write_map_assoc(K3, V3, UD, E, O,
          [$,|write_map_assoc(K4, V4, UD, E, O,
          [$,|write_map_assoc(K5, V5, UD, E, O,
          [$,|write_map_assoc(K6, V6, UD, E, O, [$]|Tl])])])])])])];
      [{K1,V1},{K2,V2},{K3,V3},{K4,V4},{K5,V5},{K6,V6},{K7,V7}] ->
        [$#,${| write_map_assoc(K1, V1, UD, E, O,
          [$,|write_map_assoc(K2, V2, UD, E, O,
          [$,|write_map_assoc(K3, V3, UD, E, O,
          [$,|write_map_assoc(K4, V4, UD, E, O,
          [$,|write_map_assoc(K5, V5, UD, E, O,
          [$,|write_map_assoc(K6, V6, UD, E, O,
          [$,|write_map_assoc(K7, V7, UD, E, O, [$]|Tl])])])])])])])];
      [{K1,V1},{K2,V2},{K3,V3},{K4,V4},{K5,V5},{K6,V6},{K7,V7},{K8,V8}] ->
        [$#,${| write_map_assoc(K1, V1, UD, E, O,
          [$,|write_map_assoc(K2, V2, UD, E, O,
          [$,|write_map_assoc(K3, V3, UD, E, O,
          [$,|write_map_assoc(K4, V4, UD, E, O,
          [$,|write_map_assoc(K5, V5, UD, E, O,
          [$,|write_map_assoc(K6, V6, UD, E, O,
          [$,|write_map_assoc(K7, V7, UD, E, O,
          [$,|write_map_assoc(K8, V8, UD, E, O, [$]|Tl])])])])])])])])]
    end;
write_map_unlimited_depth(Map, E, O, Tl) ->
    I = maps:iterator(Map, O),
    {K, V, NextI} = maps:next(I),
    [$#,${ | write_map_assoc(K, V, -1, E, O, write_map_body_unlimited_depth(NextI, E, O, Tl))].

write_map_body_unlimited_depth(I, E, O, Tl) ->
    case maps:next(I) of
        {K, V, NextI} ->
            [$, | write_map_assoc(K, V, -1, E, O, write_map_body_unlimited_depth(NextI, E, O, Tl))];
        none ->
            [$}|Tl]
    end.

write_map_assoc(K, V, D, E, O, Tl) ->
    write1(K, D, E, O, [" => "|write1(V, D, E, O, Tl)]).

write_binary_acc(<<>>, _D, Tl) ->
    ?prepend("<<>>",Tl);
write_binary_acc(B, D, Tl) when is_integer(D) ->
    Str = write_binary_no_rest(B, D),
    [Str|Tl].

-doc false.
write_binary(B, D, T) ->
    write_binary(B, D, T, []).
write_binary(B, D, T, Tl) when is_integer(T) ->
    write_binary_body(B, D, tsub(T, 4), Tl, fun (S,Rest) -> {[[$<,$<|lists:reverse(S)]|">>"], Rest} end).
write_binary_no_rest(B, D) ->
    write_binary_body(B, D, -1, [], fun (S,_) -> [[$<,$<|lists:reverse(S)]|">>"] end).

% Avoid allocating intermediate pairs by using a continuation passing style
write_binary_body(<<>> = B, _D, _T, Acc, Cont) ->
    Cont(Acc, B);
write_binary_body(B, 1, _T, Acc, Cont) ->
    Cont(?prepend("...",Acc), B);
write_binary_body(B, _D, 0, Acc, Cont) ->
    Cont(?prepend("...",Acc), B);
write_binary_body(<<X:8>>, _D, _T, Acc, Cont) ->
    Cont([int_to_str(X)|Acc], <<>>);
write_binary_body(<<X:8,Rest/bitstring>>, D, -1, Acc, Cont) ->
    S = int_to_str(X),
    write_binary_body(Rest, D-1, -1, [$,,S|Acc], Cont);
write_binary_body(<<X:8,Rest/bitstring>>, D, T, Acc, Cont) ->
    S = int_to_str(X),
    write_binary_body(Rest, D-1, tsub(T, length(S) + 1), [$,,S|Acc], Cont);
write_binary_body(B, _D, _T, Acc, Cont) ->
    L = bit_size(B),
    <<X:L>> = B,
    % Once done, the list will be reversed, so the colon at the beginning here
    % will move to the middle of the two int-strings
    Cont([[$:|int_to_str(L)],int_to_str(X)|Acc], <<>>).

%% Make sure T does not change sign.
tsub(T, _) when T < 0 -> T;
tsub(T, E) when T >= E -> T - E;
tsub(_, _) -> 0.

get_option(Key, TupleList, Default) ->
    case lists:keyfind(Key, 1, TupleList) of
	false -> Default;
	{Key, Value} -> Value;
	_ -> Default
    end.

%%% There are two functions to write Unicode atoms:
%%% - they both escape control characters < 160;
%%% - write_atom() never escapes characters >= 160;
%%% - write_atom_as_latin1() also escapes characters >= 255.

%% write_atom(Atom) -> [Char]
%%  Generate the list of characters needed to print an atom.

-doc "Returns the list of characters needed to print atom `Atom`.".
-spec write_atom(Atom) -> chars() when
      Atom :: atom().

write_atom(Atom) ->
    write_possibly_quoted_atom(Atom, false).
write_atom_acc(Atom, Tl) ->
    write_possibly_quoted_atom_acc(Atom, false, Tl).

-doc """
Returns the list of characters needed to print atom `Atom`. Non-Latin-1
characters are escaped.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec write_atom_as_latin1(Atom) -> latin1_string() when
      Atom :: atom().

write_atom_as_latin1(Atom) ->
    write_possibly_quoted_atom(Atom, true).
write_atom_as_latin1_acc(Atom, Tl) ->
    write_possibly_quoted_atom_acc(Atom, true, Tl).

write_possibly_quoted_atom(ok, _Latin1) -> "ok";
write_possibly_quoted_atom(error, _Latin1) -> "error";
write_possibly_quoted_atom(true, _Latin1) -> "true";
write_possibly_quoted_atom(false, _Latin1) -> "false";
write_possibly_quoted_atom(value, _Latin1) -> "value";
write_possibly_quoted_atom(undefined, _Latin1) -> "undefined";
write_possibly_quoted_atom('EXIT', _Latin1) -> "'EXIT'";
write_possibly_quoted_atom(Atom, Latin1) ->
    Chars = atom_to_list(Atom),
    case quote_atom(Atom, Chars) of
        true ->
            case Latin1 of
                true -> write_string_as_latin1(Chars, $');   %'
                false -> write_string(Chars, $')   %'
            end;
        false ->
            Chars
    end.

write_possibly_quoted_atom_acc(ok, _Latin1, []) -> "ok";
write_possibly_quoted_atom_acc(error, _Latin1, []) -> "error";
write_possibly_quoted_atom_acc(true, _Latin1, []) -> "true";
write_possibly_quoted_atom_acc(false, _Latin1, []) -> "false";
write_possibly_quoted_atom_acc(value, _Latin1, []) -> "value";
write_possibly_quoted_atom_acc(undefined, _Latin1, []) -> "undefined";
write_possibly_quoted_atom_acc('EXIT', _Latin1, []) -> "'EXIT'";
write_possibly_quoted_atom_acc(ok, _Latin1, Tl) -> [$o,$k|Tl];
write_possibly_quoted_atom_acc(error, _Latin1, Tl) -> ["error"|Tl];
write_possibly_quoted_atom_acc(true, _Latin1, Tl) -> ["true"|Tl];
write_possibly_quoted_atom_acc(false, _Latin1, Tl) -> ["false"|Tl];
write_possibly_quoted_atom_acc(value, _Latin1, Tl) -> ["value"|Tl];
write_possibly_quoted_atom_acc(undefined, _Latin1, Tl) -> ["undefined"|Tl];
write_possibly_quoted_atom_acc('EXIT', _Latin1, Tl) -> ["'EXIT'"|Tl];
write_possibly_quoted_atom_acc(Atom, Latin1, Tl) ->
    Chars = atom_to_list(Atom),
    case quote_atom(Atom, Chars) of
        true ->
            case Latin1 of
                true -> write_string_as_latin1_acc(Chars, $', Tl);   %'
                false -> write_string_acc(Chars, $', Tl)   %'
            end;
        false ->
            ?prepend(Chars,Tl)
    end.

%% quote_atom(Atom, CharList)
%%  Return 'true' if atom with chars in CharList needs to be quoted, else
%%  return 'false'. Notice that characters >= 160 are always quoted.

-doc false.
-spec quote_atom(atom(), chars()) -> boolean().

quote_atom(Atom, Cs0) ->
    case erl_scan:reserved_word(Atom) of
	true -> true;
	false ->
	    case Cs0 of
        [] -> true;
		[C|Cs] when is_integer(C), C >= $a, C =< $z ->
		    not name_chars(Cs);
		[C|Cs] when is_integer(C), C >= $ß, C =< $ÿ, C =/= $÷ ->
		    not name_chars(Cs);
		[C|_] when is_integer(C) -> true
	    end
    end.

name_chars([]) -> true;
name_chars([C|Cs]) ->
    case name_char(C) of
	true -> name_chars(Cs);
	false -> false
    end.

name_char($_) -> true;
name_char($@) -> true;
name_char(C) when
    C >= $a, C =< $z;
    C >= $ß, C =< $ÿ, C =/= $÷;
    C >= $A, C =< $Z;
    C >= $À, C =< $Þ, C =/= $×;
    C >= $0, C =< $9 ->
        true;
name_char(_) -> false.

%%% There are two functions to write Unicode strings:
%%% - they both escape control characters < 160;
%%% - write_string() never escapes characters >= 160;
%%% - write_string_as_latin1() also escapes characters >= 255.

%% write_string([Char]) -> [Char]
%%  Generate the list of characters needed to print a string.

-doc "Returns the list of characters needed to print `String` as a string.".
-spec write_string(String) -> chars() when
      String :: string().

write_string(S) ->
    write_string(S, $").   %"

-doc false.
-spec write_string(string(), char()) -> chars().

write_string("", $") ->
    "\"\"";
write_string(S, Q) ->
    [Q|write_string_unicode_as_unicode(S, Q)].

write_string_acc(S, Q, Tl) ->
    [Q|write_string_unicode_as_unicode_acc(S, Q, Tl)].

%% Backwards compatibility.
-doc false.
write_unicode_string(S) ->
    write_string(S).

-doc "Returns the list of characters needed to print `Latin1String` as a string.".
-doc(#{since => <<"OTP R16B">>}).
-spec write_latin1_string(Latin1String) -> latin1_string() when
      Latin1String :: latin1_string().

write_latin1_string(S) ->
    write_latin1_string(S, $").   %"

-doc false.
-spec write_latin1_string(latin1_string(), char()) -> latin1_string().

write_latin1_string(S, Q) ->
    [Q|write_string1(latin1, S, Q)].

-doc """
Returns the list of characters needed to print `String` as a string. Non-Latin-1
characters are escaped.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec write_string_as_latin1(String) -> latin1_string() when
      String :: string().

write_string_as_latin1(S) ->
    write_string_as_latin1(S, $").   %"

-define(non_esc_ascii(C, Q),
    is_integer(C), C >= $\s, C =< $~, C =/= $\\, C =/= Q).

-define(all_non_esc_ascii(C1,C2,C3,C4,Q),
    ?non_esc_ascii(C1,Q),
    ?non_esc_ascii(C2,Q),
    ?non_esc_ascii(C3,Q),
    ?non_esc_ascii(C4,Q)
).

-doc false.
-spec write_string_as_latin1(string(), char()) -> latin1_string().

write_string_as_latin1(S, Q) ->
    [Q|write_string1(unicode_as_latin1, S, Q)].
write_string_as_latin1_acc(S, Q, Tl) ->
    [Q|write_string1_acc(unicode_as_latin1, S, Q, Tl)].

write_string1(_,[], Q) ->
    [Q];
write_string1(Enc,[C1,C2,C3,C4|Cs], Q) when ?all_non_esc_ascii(C1,C2,C3,C4,Q) ->
    [C1,C2,C3,C4|write_string1(Enc,Cs,Q)];
write_string1(Enc,[C|Cs], Q) when is_integer(C) ->
    string_char(Enc,C, Q, write_string1(Enc,Cs, Q)).

write_string1_acc(_,[], Q, Tl) ->
    [Q|Tl];
write_string1_acc(Enc,[C1,C2,C3,C4|Cs], Q, Tl) when ?all_non_esc_ascii(C1,C2,C3,C4,Q) ->
    [C1,C2,C3,C4|write_string1_acc(Enc,Cs,Q,Tl)];
write_string1_acc(Enc,[C|Cs], Q, Tl) when is_integer(C) ->
    string_char(Enc,C, Q, write_string1_acc(Enc,Cs, Q, Tl)).

string_char(_,Q, Q, Tail) -> [$\\,Q|Tail];	%Must check these first!
string_char(_,$\\, _, Tail) -> [$\\,$\\|Tail];
string_char(_,$\n, _, Tail) -> [$\\,$n|Tail];	%\n = LF
string_char(_,$\r, _, Tail) -> [$\\,$r|Tail];	%\r = CR
string_char(_,$\t, _, Tail) -> [$\\,$t|Tail];	%\t = TAB
string_char(_,$\v, _, Tail) -> [$\\,$v|Tail];	%\v = VT
string_char(_,$\b, _, Tail) -> [$\\,$b|Tail];	%\b = BS
string_char(_,$\f, _, Tail) -> [$\\,$f|Tail];	%\f = FF
string_char(_,$\e, _, Tail) -> [$\\,$e|Tail];	%\e = ESC
string_char(_,$\d, _, Tail) -> [$\\,$d|Tail];	%\d = DEL
string_char(_,C, _, Tail) when C >= $\s, C =< $~ ->
    [C|Tail];
string_char(latin1,C, _, Tail) when C >= $\240, C =< $\377 ->
    [C|Tail];
string_char(unicode_as_unicode,C, _, Tail) when C >= $\240 ->
    [C|Tail];
string_char(unicode_as_latin1,C, _, Tail) when C >= $\240, C =< $\377 ->
    [C|Tail];
string_char(unicode_as_latin1,C, _, Tail) when C >= $\377 ->
    [$\\, $x, ${ | erlang:integer_to_list(C,16)++[$} | Tail]];
string_char(_,C, _, Tail) when C < $\240 ->	%Other control characters.
    C1 = (C bsr 6) + $0,
    C2 = ((C bsr 3) band 7) + $0,
    C3 = (C band 7) + $0,
    [$\\,C1,C2,C3|Tail].

% Same as write_string1, but specialised to unicode for performance
write_string_unicode_as_unicode(Cs, Q) ->
    write_string_unicode_as_unicode_acc(Cs, Q, "").
write_string_unicode_as_unicode_acc([], Q, Tl) ->
    [Q|Tl];
write_string_unicode_as_unicode_acc([C1,C2,C3,C4|Cs], Q, Tl) when ?all_non_esc_ascii(C1,C2,C3,C4,Q) ->
    [C1,C2,C3,C4|write_string_unicode_as_unicode_acc(Cs, Q, Tl)];
write_string_unicode_as_unicode_acc([C|Cs], Q, Tl) when is_integer(C) ->
    string_char_unicode_as_unicode(C, Q, write_string_unicode_as_unicode_acc(Cs, Q, Tl)).

string_char_unicode_as_unicode(Q, Q, Tail) -> [$\\,Q|Tail];	%Must check these first!
string_char_unicode_as_unicode($\\, _, Tail) -> [$\\,$\\|Tail];
string_char_unicode_as_unicode($\n, _, Tail) -> [$\\,$n|Tail];	%\n = LF
string_char_unicode_as_unicode($\r, _, Tail) -> [$\\,$r|Tail];	%\r = CR
string_char_unicode_as_unicode($\t, _, Tail) -> [$\\,$t|Tail];	%\t = TAB
string_char_unicode_as_unicode($\v, _, Tail) -> [$\\,$v|Tail];	%\v = VT
string_char_unicode_as_unicode($\b, _, Tail) -> [$\\,$b|Tail];	%\b = BS
string_char_unicode_as_unicode($\f, _, Tail) -> [$\\,$f|Tail];	%\f = FF
string_char_unicode_as_unicode($\e, _, Tail) -> [$\\,$e|Tail];	%\e = ESC
string_char_unicode_as_unicode($\d, _, Tail) -> [$\\,$d|Tail];	%\d = DEL
string_char_unicode_as_unicode(C, _, Tail) when C >= $\s, C =< $~ ; C >= $\240 ->
    [C|Tail];
string_char_unicode_as_unicode(C, _, Tail) when C < $\240 ->	%Other control characters.
    C1 = (C bsr 6) + $0,
    C2 = ((C bsr 3) band 7) + $0,
    C3 = (C band 7) + $0,
    [$\\,C1,C2,C3|Tail].

%%% There are two functions to write a Unicode character:
%%% - they both escape control characters < 160;
%%% - write_char() never escapes characters >= 160;
%%% - write_char_as_latin1() also escapes characters >= 255.

%% write_char(Char) -> [char()].
%%  Generate the list of characters needed to print a character constant.
%%  Must special case SPACE, $\s, here.

-doc """
Returns the list of characters needed to print a character constant in the
Unicode character set.
""".
-spec write_char(Char) -> chars() when
      Char :: char().

write_char($\s) -> "$\\s";			%Must special case this.
write_char(C) when is_integer(C), C >= $\000 ->
    [$$|string_char(unicode_as_unicode, C, -1, [])].

%% Backwards compatibility.
-doc false.
write_unicode_char(C) ->
    write_char(C).

-doc """
Returns the list of characters needed to print a character constant in the ISO
Latin-1 character set.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec write_latin1_char(Latin1Char) -> latin1_string() when
      Latin1Char :: unicode:latin1_char().

write_latin1_char(Lat1) when is_integer(Lat1), Lat1 >= $\000, Lat1 =< $\377  ->
    [$$|string_char(latin1, Lat1, -1, [])].

-doc """
Returns the list of characters needed to print a character constant in the
Unicode character set. Non-Latin-1 characters are escaped.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec write_char_as_latin1(Char) -> latin1_string() when
      Char :: char().

write_char_as_latin1(Uni) when is_integer(Uni), Uni >= $\000 ->
    [$$|string_char(unicode_as_latin1,Uni, -1, [])].

%% latin1_char_list(CharList)
%% deep_latin1_char_list(CharList)
%%  Return true if CharList is a (possibly deep) list of Latin-1
%%  characters, else false.

-doc """
Returns `true` if `Term` is a flat list of characters in the ISO Latin-1 range,
otherwise `false`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec latin1_char_list(Term) -> boolean() when
      Term :: term().

latin1_char_list([]) -> true;
latin1_char_list([C|Cs]) when is_integer(C), C >= $\000, C =< $\377 ->
    latin1_char_list(Cs);
latin1_char_list(_) -> false.			%Everything else is false

-doc """
Returns `true` if `Term` is a flat list of characters in the Unicode range,
otherwise `false`.
""".
-spec char_list(Term) -> boolean() when
      Term :: term().

char_list([]) -> true;
char_list([C|Cs])
    when is_integer(C), C >= 0, C < 16#D800;
         is_integer(C), C > 16#DFFF, C < 16#FFFE;
         is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    char_list(Cs);
char_list(_) -> false.			%Everything else is false

-doc """
Returns `true` if `Term` is a, possibly deep, list of characters in the ISO
Latin-1 range, otherwise `false`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec deep_latin1_char_list(Term) -> boolean() when
      Term :: term().

deep_latin1_char_list(Cs) ->
    deep_latin1_char_list(Cs, []).

deep_latin1_char_list([], [Cs|More]) ->
    deep_latin1_char_list(Cs, More);
deep_latin1_char_list([], []) ->
    true;
deep_latin1_char_list([C|Cs], More) when is_list(C) ->
    deep_latin1_char_list(C, [Cs|More]);
deep_latin1_char_list([C|Cs], More) when is_integer(C), C >= $\000, C =< $\377 ->
    deep_latin1_char_list(Cs, More);
deep_latin1_char_list(_, _More) ->			%Everything else is false
    false.

-doc """
Returns `true` if `Term` is a, possibly deep, list of characters in the Unicode
range, otherwise `false`.
""".
-spec deep_char_list(Term) -> boolean() when
      Term :: term().

deep_char_list(Cs) ->
    deep_char_list(Cs, []).

deep_char_list([], [Cs|More]) ->
    deep_char_list(Cs, More);
deep_char_list([], []) ->
    true;
deep_char_list([C|Cs], More) when is_list(C) ->
    deep_char_list(C, [Cs|More]);
deep_char_list([C|Cs], More)
  when is_integer(C), C >= 0, C < 16#D800;
       is_integer(C), C > 16#DFFF, C < 16#FFFE;
       is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    deep_char_list(Cs, More);
deep_char_list(_, _More) ->		%Everything else is false
    false.

-doc false.
deep_unicode_char_list(Term) ->
    deep_char_list(Term).

%% printable_latin1_list([Char]) -> boolean()
%%  Return true if CharList is a list of printable Latin1 characters, else
%%  false.

-doc """
Returns `true` if `Term` is a flat list of printable ISO Latin-1 characters,
otherwise `false`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec printable_latin1_list(Term) -> boolean() when
      Term :: term().

% is_integer(C), C >= $\040, C =< $\176;
% is_integer(C), C >= $\240, C =< $\377
-define(is_printable_latin1(X), (
    (X =:= $\n) orelse (X =:= $\r) orelse (X =:= $\t) orelse (X =:= $\v) orelse
    (X =:= $\b) orelse (X =:= $\f) orelse (X =:= $\e) orelse

    % ----

    (X =:= 32) orelse
    (X =:= 33) orelse (X =:= 34) orelse (X =:= 35) orelse (X =:= 36) orelse
    (X =:= 37) orelse (X =:= 38) orelse (X =:= 39) orelse (X =:= 40) orelse
    (X =:= 41) orelse (X =:= 42) orelse (X =:= 43) orelse (X =:= 44) orelse
    (X =:= 45) orelse (X =:= 46) orelse (X =:= 47) orelse (X =:= 48) orelse
    (X =:= 49) orelse (X =:= 50) orelse (X =:= 51) orelse (X =:= 52) orelse
    (X =:= 53) orelse (X =:= 54) orelse (X =:= 55) orelse (X =:= 56) orelse
    (X =:= 57) orelse (X =:= 58) orelse (X =:= 59) orelse (X =:= 60) orelse
    (X =:= 61) orelse (X =:= 62) orelse (X =:= 63) orelse (X =:= 64) orelse
    (X =:= 65) orelse (X =:= 66) orelse (X =:= 67) orelse (X =:= 68) orelse
    (X =:= 69) orelse (X =:= 70) orelse (X =:= 71) orelse (X =:= 72) orelse
    (X =:= 73) orelse (X =:= 74) orelse (X =:= 75) orelse (X =:= 76) orelse
    (X =:= 77) orelse (X =:= 78) orelse (X =:= 79) orelse (X =:= 80) orelse
    (X =:= 81) orelse (X =:= 82) orelse (X =:= 83) orelse (X =:= 84) orelse
    (X =:= 85) orelse (X =:= 86) orelse (X =:= 87) orelse (X =:= 88) orelse
    (X =:= 89) orelse (X =:= 90) orelse (X =:= 91) orelse (X =:= 92) orelse
    (X =:= 93) orelse (X =:= 94) orelse (X =:= 95) orelse (X =:= 96) orelse
    (X =:= 97) orelse (X =:= 98) orelse (X =:= 99) orelse (X =:= 100) orelse
    (X =:= 101) orelse (X =:= 102) orelse (X =:= 103) orelse (X =:= 104) orelse
    (X =:= 105) orelse (X =:= 106) orelse (X =:= 107) orelse (X =:= 108) orelse
    (X =:= 109) orelse (X =:= 110) orelse (X =:= 111) orelse (X =:= 112) orelse
    (X =:= 113) orelse (X =:= 114) orelse (X =:= 115) orelse (X =:= 116) orelse
    (X =:= 117) orelse (X =:= 118) orelse (X =:= 119) orelse (X =:= 120) orelse
    (X =:= 121) orelse (X =:= 122) orelse (X =:= 123) orelse (X =:= 124) orelse
    (X =:= 125) orelse (X =:= 126) orelse

    % ----

    (X =:= 160) orelse (X =:= 161) orelse (X =:= 162) orelse (X =:= 163) orelse
    (X =:= 164) orelse (X =:= 165) orelse (X =:= 166) orelse (X =:= 167) orelse
    (X =:= 168) orelse (X =:= 169) orelse (X =:= 170) orelse (X =:= 171) orelse
    (X =:= 172) orelse (X =:= 173) orelse (X =:= 174) orelse (X =:= 175) orelse
    (X =:= 176) orelse (X =:= 177) orelse (X =:= 178) orelse (X =:= 179) orelse
    (X =:= 180) orelse (X =:= 181) orelse (X =:= 182) orelse (X =:= 183) orelse
    (X =:= 184) orelse (X =:= 185) orelse (X =:= 186) orelse (X =:= 187) orelse
    (X =:= 188) orelse (X =:= 189) orelse (X =:= 190) orelse (X =:= 191) orelse
    (X =:= 192) orelse (X =:= 193) orelse (X =:= 194) orelse (X =:= 195) orelse
    (X =:= 196) orelse (X =:= 197) orelse (X =:= 198) orelse (X =:= 199) orelse
    (X =:= 200) orelse (X =:= 201) orelse (X =:= 202) orelse (X =:= 203) orelse
    (X =:= 204) orelse (X =:= 205) orelse (X =:= 206) orelse (X =:= 207) orelse
    (X =:= 208) orelse (X =:= 209) orelse (X =:= 210) orelse (X =:= 211) orelse
    (X =:= 212) orelse (X =:= 213) orelse (X =:= 214) orelse (X =:= 215) orelse
    (X =:= 216) orelse (X =:= 217) orelse (X =:= 218) orelse (X =:= 219) orelse
    (X =:= 220) orelse (X =:= 221) orelse (X =:= 222) orelse (X =:= 223) orelse
    (X =:= 224) orelse (X =:= 225) orelse (X =:= 226) orelse (X =:= 227) orelse
    (X =:= 228) orelse (X =:= 229) orelse (X =:= 230) orelse (X =:= 231) orelse
    (X =:= 232) orelse (X =:= 233) orelse (X =:= 234) orelse (X =:= 235) orelse
    (X =:= 236) orelse (X =:= 237) orelse (X =:= 238) orelse (X =:= 239) orelse
    (X =:= 240) orelse (X =:= 241) orelse (X =:= 242) orelse (X =:= 243) orelse
    (X =:= 244) orelse (X =:= 245) orelse (X =:= 246) orelse (X =:= 247) orelse
    (X =:= 248) orelse (X =:= 249) orelse (X =:= 250) orelse (X =:= 251) orelse
    (X =:= 252) orelse (X =:= 253) orelse (X =:= 254) orelse (X =:= 255))).

-define(all_printable_latin1(C1, C2, C3, C4, C5, C6, C7, C8),
    (?is_printable_latin1(C1)), (?is_printable_latin1(C2)), (?is_printable_latin1(C3)),
    (?is_printable_latin1(C4)), (?is_printable_latin1(C5)), (?is_printable_latin1(C6)),
    (?is_printable_latin1(C7)), (?is_printable_latin1(C8))
).

printable_latin1_list([]) ->
    true;
printable_latin1_list([C1,C2,C3,C4,C5,C6,C7,C8]) when ?all_printable_latin1(C1,C2,C3,C4,C5,C6,C7,C8) ->
    true;
printable_latin1_list([C1,C2,C3,C4,C5,C6,C7,C8|Cs]) when ?all_printable_latin1(C1,C2,C3,C4,C5,C6,C7,C8) ->
    printable_latin1_list(Cs);
printable_latin1_list([C1]) when
        ?is_printable_latin1(C1) ->
    true;
printable_latin1_list([C1,C2]) when
        ?is_printable_latin1(C1), ?is_printable_latin1(C2) ->
    true;
printable_latin1_list([C1,C2,C3]) when
        ?is_printable_latin1(C1), ?is_printable_latin1(C2), ?is_printable_latin1(C3) ->
    true;
printable_latin1_list([C1,C2,C3,C4]) when
        ?is_printable_latin1(C1), ?is_printable_latin1(C2),
        ?is_printable_latin1(C3), ?is_printable_latin1(C4) ->
    true;
printable_latin1_list([C1,C2,C3,C4,C5]) when
        ?is_printable_latin1(C1), ?is_printable_latin1(C2),
        ?is_printable_latin1(C3), ?is_printable_latin1(C4),
        ?is_printable_latin1(C5) ->
    true;
printable_latin1_list([C1,C2,C3,C4,C5,C6]) when
        ?is_printable_latin1(C1), ?is_printable_latin1(C2),
        ?is_printable_latin1(C3), ?is_printable_latin1(C4),
        ?is_printable_latin1(C5), ?is_printable_latin1(C6) ->
    true;
printable_latin1_list([C1,C2,C3,C4,C5,C6,C7]) when
        ?is_printable_latin1(C1), ?is_printable_latin1(C2),
        ?is_printable_latin1(C3), ?is_printable_latin1(C4),
        ?is_printable_latin1(C5), ?is_printable_latin1(C6),
        ?is_printable_latin1(C7) ->
    true;
printable_latin1_list(_) -> false.			%Everything else is false

%% printable_list([Char]) -> boolean()
%%  Return true if CharList is a list of printable characters, else
%%  false. The notion of printable in Unicode terms is somewhat floating.
%%  Everything that is not a control character and not invalid unicode
%%  will be considered printable.
%%  What the user has noted as printable characters is what actually
%%  specifies when this function will return true. If the VM is started
%%  with +pc latin1, only the latin1 range will be deemed as printable
%%  if on the other hand +pc unicode is given, all characters in the Unicode
%%  character set are deemed printable. latin1 is default.

-doc """
Returns `true` if `Term` is a flat list of printable characters, otherwise
`false`.

What is a printable character in this case is determined by startup flag `+pc`
to the Erlang VM; see `io:printable_range/0` and
[`erl(1)`](`e:erts:erl_cmd.md`).
""".
-spec printable_list(Term) -> boolean() when
      Term :: term().

printable_list(L) ->
    %% There will be more alternatives returns from io:printable range
    %% in the future. To not have a catch-all clause is deliberate.
    case io:printable_range() of
	latin1 ->
	    printable_latin1_list(L);
	unicode ->
	    printable_unicode_list(L)
    end.

-define(is_printable_unicode(C),
    ((C =:= $\n) orelse (C =:= $\r) orelse (C =:= $\t) orelse (C =:= $\v) orelse
    (C =:= $\b) orelse (C =:= $\f) orelse (C =:= $\e)) orelse
    (is_integer(C) andalso
        (C >= $\040 andalso C =< $\176) orelse
        (C >= 16#A0 andalso C < 16#D800) orelse
        (C > 16#DFFF andalso C < 16#FFFE) orelse
        (C > 16#FFFF andalso C =< 16#10FFFF)
    )
).

-define(all_printable_unicode(C1, C2, C3, C4, C5, C6, C7, C8),
    (?is_printable_unicode(C1)), (?is_printable_unicode(C2)), (?is_printable_unicode(C3)),
    (?is_printable_unicode(C4)), (?is_printable_unicode(C5)), (?is_printable_unicode(C6)),
    (?is_printable_unicode(C7)), (?is_printable_unicode(C8))
).

-doc """
Returns `true` if `Term` is a flat list of printable Unicode characters,
otherwise `false`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec printable_unicode_list(Term) -> boolean() when
      Term :: term().

printable_unicode_list([]) ->
    true;
printable_unicode_list([C1,C2,C3,C4,C5,C6,C7,C8]) when ?all_printable_unicode(C1,C2,C3,C4,C5,C6,C7,C8) ->
    true;
printable_unicode_list([C1,C2,C3,C4,C5,C6,C7,C8|Cs]) when ?all_printable_unicode(C1,C2,C3,C4,C5,C6,C7,C8) ->
    printable_unicode_list(Cs);
printable_unicode_list([C1]) when
        ?is_printable_unicode(C1) ->
    true;
printable_unicode_list([C1,C2]) when
        ?is_printable_unicode(C1), ?is_printable_unicode(C2) ->
    true;
printable_unicode_list([C1,C2,C3]) when
        ?is_printable_unicode(C1), ?is_printable_unicode(C2), ?is_printable_unicode(C3) ->
    true;
printable_unicode_list([C1,C2,C3,C4]) when
        ?is_printable_unicode(C1), ?is_printable_unicode(C2),
        ?is_printable_unicode(C3), ?is_printable_unicode(C4) ->
    true;
printable_unicode_list([C1,C2,C3,C4,C5]) when
        ?is_printable_unicode(C1), ?is_printable_unicode(C2),
        ?is_printable_unicode(C3), ?is_printable_unicode(C4),
        ?is_printable_unicode(C5) ->
    true;
printable_unicode_list([C1,C2,C3,C4,C5,C6]) when
        ?is_printable_unicode(C1), ?is_printable_unicode(C2),
        ?is_printable_unicode(C3), ?is_printable_unicode(C4),
        ?is_printable_unicode(C5), ?is_printable_unicode(C6) ->
    true;
printable_unicode_list([C1,C2,C3,C4,C5,C6,C7]) when
        ?is_printable_unicode(C1), ?is_printable_unicode(C2),
        ?is_printable_unicode(C3), ?is_printable_unicode(C4),
        ?is_printable_unicode(C5), ?is_printable_unicode(C6),
        ?is_printable_unicode(C7) ->
    true;
printable_unicode_list(_) -> false.			%Everything else is false

%% List = nl()
%%  Return a list of characters to generate a newline.

-doc "Returns a character list that represents a new line character.".
-spec nl() -> string().

nl() ->
    "\n".

%%
%% Utilities for collecting characters in input files
%%

count_and_find_utf8(Bin,N) ->
    cafu(Bin,N,0,0,none).

cafu(<<>>,_N,Count,_ByteCount,SavePos) ->
    {Count,SavePos};
cafu(<<_/utf8,Rest/binary>>, 0, Count, ByteCount, _SavePos) ->
    cafu(Rest,-1,Count+1,0,ByteCount);
cafu(<<_/utf8,Rest/binary>>, N, Count, _ByteCount, SavePos) when N < 0 ->
    cafu(Rest,-1,Count+1,0,SavePos);
cafu(<<_/utf8,Rest/binary>> = Whole, N, Count, ByteCount, SavePos) ->
    Delta = byte_size(Whole) - byte_size(Rest),
    cafu(Rest,N-1,Count+1,ByteCount+Delta,SavePos);
cafu(_Other,_N,Count,_ByteCount,SavePos) -> % Non Utf8 character at end
    {Count,SavePos}.

%% collect_chars(State, Data, Count). New in R9C.
%%  Returns:
%%      {stop,Result,RestData}
%%      NewState
%%% BC (with pre-R13).
-doc false.
collect_chars(Tag, Data, N) ->
    collect_chars(Tag, Data, latin1, N).

%% Now we are aware of encoding...
-doc false.
collect_chars(start, Data, unicode, N) when is_binary(Data), is_integer(N) ->
    {Size,Npos} = count_and_find_utf8(Data,N),
    if Size > N ->
	    {B1,B2} = split_binary(Data, Npos),
	    {stop,B1,B2};
       Size < N ->
	    {binary,[Data],N-Size};
       true ->
	    {stop,Data,<<>>}
    end;
collect_chars(start, Data, latin1, N) when is_binary(Data), is_integer(N) ->
    Size = byte_size(Data),
    if Size > N ->
	    {B1,B2} = split_binary(Data, N),
	    {stop,B1,B2};
       Size < N ->
	    {binary,[Data],N-Size};
       true ->
	    {stop,Data,<<>>}
    end;
collect_chars(start, eof, _,_) ->
    {stop,eof,eof};
collect_chars({binary,[<<>>],_N}, eof, _,_) ->
    {stop,eof,eof};
collect_chars({binary,Stack,_N}, eof, _,_) ->
    {stop,binrev(Stack),eof};
collect_chars(start,Data,_,N) when is_list(Data), is_integer(N) ->
    collect_chars_list([], N, Data);
collect_chars({binary,Stack,N}, Data,unicode, _) when is_integer(N) ->
    {Size,Npos} = count_and_find_utf8(Data,N),
    if Size > N ->
	    {B1,B2} = split_binary(Data, Npos),
	    {stop,binrev(Stack, [B1]),B2};
       Size < N ->
	    {binary,[Data|Stack],N-Size};
       true ->
	    {stop,binrev(Stack, [Data]),<<>>}
    end;
collect_chars({binary,Stack,N}, Data,latin1, _) when is_integer(N) ->
    Size = byte_size(Data),
    if Size > N ->
	    {B1,B2} = split_binary(Data, N),
	    {stop,binrev(Stack, [B1]),B2};
       Size < N ->
	    {binary,[Data|Stack],N-Size};
       true ->
	    {stop,binrev(Stack, [Data]),<<>>}
    end;
collect_chars({list,Stack,N}, Data, _,_) when is_integer(N) ->
    collect_chars_list(Stack, N, Data);

%% collect_chars(Continuation, MoreChars, Count)
%%  Returns:
%%	{done,Result,RestChars}
%%	{more,Continuation}

collect_chars([], Chars, _, N) when is_integer(N) ->
    collect_chars1(N, Chars, []);
collect_chars({Left,Sofar}, Chars, _, _N) when is_integer(Left) ->
    collect_chars1(Left, Chars, Sofar).

collect_chars1(N, Chars, Stack) when N =< 0 ->
    {done,lists:reverse(Stack),Chars};
collect_chars1(N, [C|Rest], Stack) ->
    collect_chars1(N-1, Rest, [C|Stack]);
collect_chars1(_N, eof, []) ->
    {done,eof,[]};
collect_chars1(_N, eof, Stack) ->
    {done,lists:reverse(Stack),[]};
collect_chars1(N, [], Stack) ->
    {more,{N,Stack}}.

collect_chars_list(Stack, 0, Data) ->
    {stop,lists:reverse(Stack),Data};
collect_chars_list([], _N, eof) ->
    {stop,eof,eof};
collect_chars_list(Stack, _N, eof) ->
    {stop,lists:reverse(Stack),eof};
collect_chars_list(Stack, N, []) ->
    {list,Stack,N};
collect_chars_list(Stack,N, [H|T]) ->
    collect_chars_list([H|Stack], N-1, T).

%% collect_line(State, Data, _). New in R9C.
%%  Returns:
%%	{stop,Result,RestData}
%%	NewState
%%% BC (with pre-R13).
-doc false.
collect_line(Tag, Data, Any) ->
    collect_line(Tag, Data, latin1, Any).

%% Now we are aware of encoding...
-doc false.
collect_line(start, Data, Encoding, _) when is_binary(Data) ->
    collect_line_bin(Data, Data, [], Encoding);
collect_line(start, Data, _, _) when is_list(Data) ->
    collect_line_list(Data, []);
collect_line(start, eof, _, _) ->
    {stop,eof,eof};
collect_line(Stack, Data, Encoding, _) when is_binary(Data) ->
    collect_line_bin(Data, Data, Stack, Encoding);
collect_line(Stack, Data, _, _) when is_list(Data) ->
    collect_line_list(Data, Stack);
collect_line([B|_]=Stack, eof, _, _) when is_binary(B) ->
    {stop,binrev(Stack),eof};
collect_line(Stack, eof, _, _) ->
    {stop,lists:reverse(Stack),eof}.


collect_line_bin(<<$\n,T/binary>>, Data, Stack0, _) ->
    N = byte_size(Data) - byte_size(T),
    <<Line:N/binary,_/binary>> = Data,
    case Stack0 of
	[] ->
	    {stop,Line,T};
	[<<$\r>>|Stack] when N =:= 1 ->
	    {stop,binrev(Stack, [$\n]),T};
	_ ->
	    {stop,binrev(Stack0, [Line]),T}
    end;
collect_line_bin(<<$\r,$\n,T/binary>>, Data, Stack, _) ->
    N = byte_size(Data) - byte_size(T) - 2,
    <<Line:N/binary,_/binary>> = Data,
    {stop,binrev(Stack, [Line,$\n]),T};
collect_line_bin(<<$\r>>, Data0, Stack, _) ->
    N = byte_size(Data0) - 1,
    <<Data:N/binary,_/binary>> = Data0,
    [<<$\r>>,Data|Stack];
collect_line_bin(<<_,T/binary>>, Data, Stack, Enc) ->
    collect_line_bin(T, Data, Stack, Enc);
collect_line_bin(<<>>, Data, Stack, _) ->
    %% Need more data here.
    [Data|Stack].

collect_line_list([$\n|T], [$\r|Stack]) ->
    {stop,lists:reverse(Stack, [$\n]),T};
collect_line_list([$\n|T], Stack) ->
    {stop,lists:reverse(Stack, [$\n]),T};
collect_line_list([H|T], Stack) ->
    collect_line_list(T, [H|Stack]);
collect_line_list([], Stack) ->
    Stack.

%% Translator function to emulate a new (R9C and later)
%% I/O client when you have an old one.
%%
%% Implements a middleman that is get_until server and get_chars client.

%%% BC (with pre-R13).
-doc false.
get_until(Any,Data,Arg) ->
    get_until(Any,Data,latin1,Arg).

%% Now we are aware of encoding...
-doc false.
get_until(start, Data, Encoding, XtraArg) ->
    %% We use the type of the initial data as an indicator of what
    %% the final result should be cast to. We cannot use the final
    %% data as that might be eof and then we have no idea what to
    %% convert to.
    get_until({is_binary(Data), []}, Data, Encoding, XtraArg);
get_until({IsDataBinary, Cont}, Data, Encoding, {Mod, Func, XtraArgs}) ->
    Chars = if is_binary(Data), Encoding =:= unicode ->
		    unicode:characters_to_list(Data,utf8);
	       is_binary(Data) ->
		    binary_to_list(Data);
	       true ->
		    Data
	    end,
    case apply(Mod, Func, [Cont,Chars|XtraArgs]) of
	{done,Result,Buf} ->
	    {stop,if IsDataBinary,
		     is_list(Result),
		     Encoding =:= unicode ->
			  unicode:characters_to_binary(Result,unicode,unicode);
		     IsDataBinary,
		     is_list(Result) ->
			  erlang:iolist_to_binary(Result);
%%		     IsDataBinary,
%%		     is_list(Result),
%% 		     Encoding =:= latin1 ->
%% 			  % Should check for only latin1, but skip that for
%% 			  % efficiency reasons.
%% 			  [ exit({cannot_convert, unicode, latin1}) ||
%% 			      X <- List, X > 255 ];
		     true ->
			  Result
		  end,
	     Buf};
	{more,NewCont} ->
	    {IsDataBinary, NewCont}
    end.

binrev(L) ->
    list_to_binary(lists:reverse(L)).

binrev(L, T) ->
    list_to_binary(lists:reverse(L, T)).

-doc false.
-spec limit_term(term(), depth()) -> term().

%% The intention is to mimic the depth limitation of io_lib:write()
%% and io_lib_pretty:print(). The leaves ('...') should never be
%% seen when printed with the same depth. Bitstrings are never
%% truncated, which is OK as long as they are not sent to other nodes.
limit_term(Term, Depth) when is_integer(Depth), Depth < 0 ->
    Term;
limit_term(Term, Depth) when is_integer(Depth) ->
    try test_limit(Term, Depth) of
        ok -> Term
    catch
        throw:limit ->
            limit(Term, Depth)
    end.

limit(_, 0) -> '...';
limit([_|_], 1) ->
	['...'];
limit([H|T]=L, D) ->
    case printable_list(L) of
        true -> L;
        false ->
            [limit(H, D-1)|limit_tail(T, D-1)]
    end;
limit(#{}=Term, D) ->
    limit_map(Term, D);
limit({}=T, _D) -> T;
limit(Term, D) when is_bitstring(Term) ->
    limit_bitstring(Term, D);
limit(T, D) when is_tuple(T) ->
    if
	D =:= 1 -> {'...'};
	true ->
            list_to_tuple([limit(element(1, T), D-1)|
                           limit_tuple(T, 2, D-1)])
    end;
limit(Term, _D) -> Term.

limit_tail([], _D) -> [];
limit_tail(_, 1) -> ['...'];
limit_tail([H|T], D) ->
    [limit(H, D-1)|limit_tail(T, D-1)];
limit_tail(Other, D) ->
    limit(Other, D-1).

limit_tuple(T, I, _D) when I > tuple_size(T) -> [];
limit_tuple(_, _I, 1) -> ['...'];
limit_tuple(T, I, D) ->
    [limit(element(I, T), D-1)|limit_tuple(T, I+1, D-1)].

%% Cannot limit maps properly since there is no guarantee that
%% maps:from_list() creates a map with the same internal ordering of
%% the selected associations as in Map. Instead of subtracting one
%% from the depth as the map associations are traversed (as is done
%% for tuples and lists), the same depth is applied to each and every
%% (returned) association.
limit_map(Map, D) ->
    %% Keep one extra association to make sure the final ',...' is included.
    limit_map_body(maps:iterator(Map), D + 1, D, []).

limit_map_body(_I, 0, _D0, Acc) ->
    maps:from_list(Acc);
limit_map_body(I, D, D0, Acc) ->
    case maps:next(I) of
        {K, V, NextI} ->
            limit_map_body(NextI, D-1, D0, [limit_map_assoc(K, V, D0) | Acc]);
        none ->
            maps:from_list(Acc)
    end.

limit_map_assoc(K, V, D) ->
    %% Keep keys as are to avoid creating duplicated keys.
    {K, limit(V, D - 1)}.

limit_bitstring(B, _D) -> B. % Keeps all printable binaries.

test_limit(_, 0) -> throw(limit);
test_limit([_|_], 1) ->
	throw(limit);
test_limit([H|T]=L, D) when is_integer(D) ->
    case printable_list(L) of
        true -> ok;
        false ->
            D1=D-1,
            test_limit(H, D1),
            test_limit_tail(T, D1)
    end;
test_limit({}, _D) -> ok;
test_limit(#{}=Term, D) ->
    test_limit_map(Term, D);
test_limit(Term, D) when is_bitstring(Term) ->
    test_limit_bitstring(Term, D);
test_limit(T, D) when is_tuple(T) ->
    test_limit_tuple(T, 1, tuple_size(T), D);
test_limit(_Term, _D) -> ok.

test_limit_tail([], _D) -> ok;
test_limit_tail(_, 1) -> throw(limit);
test_limit_tail([H|T], D) ->
    D1=D-1,
    test_limit(H, D1),
    test_limit_tail(T, D1);
test_limit_tail(Other, D) ->
    test_limit(Other, D-1).

test_limit_tuple(_T, I, Sz, _D) when I > Sz -> ok;
test_limit_tuple(_, _, _, 1) -> throw(limit);
test_limit_tuple(T, I, Sz, D) ->
    D1=D-1,
    test_limit(element(I, T), D1),
    test_limit_tuple(T, I+1, Sz, D1).

test_limit_map(Map, D) ->
    test_limit_map_body(maps:iterator(Map), D).

test_limit_map_body(_I, 0) -> throw(limit); % cannot happen
test_limit_map_body(I, D) ->
    case maps:next(I) of
        {K, V, NextI} ->
            test_limit_map_assoc(K, V, D),
            test_limit_map_body(NextI, D-1);
        none ->
            ok
    end.

test_limit_map_assoc(K, V, D) ->
    D1=D-1,
    test_limit(K, D1),
    test_limit(V, D1).

test_limit_bitstring(_, _) -> ok.

-doc false.
-spec chars_length(chars()) -> non_neg_integer().
%% Optimized for deep lists S such that deep_latin1_char_list(S) is
%% true. No binaries allowed! It is assumed that $\r is never followed
%% by $\n if S is an iolist() (string:length() assigns such a
%% sub-sequence length 1).
chars_length(S) ->
    try
        %% true = deep_latin1_char_list(S),
        iolist_size(S)
    catch
        _:_ ->
            string:length(S)
    end.

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
