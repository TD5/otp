-module(big_maps).

-export([f/0]).

-type alphabet() :: a | b | c | d | e | f | g | h | i | j | k | l | m | n.
-type numbers() :: one | two | three | four | five | six | seven | eight | nine | ten | eleven | twelve | thirteen | fourteen.

-type big_map() :: #{ a => true | undefined,
                      one => false | undefined
                    }.

mk_big_map() ->
  #{ a =>
         case rand:uniform(9) of
           1 -> true;
           _ -> undefined
         end,
     one =>
         case rand:uniform(9) of
           1 -> false;
           _ -> undefined
         end
    }.

p() -> {a,{a,{a,{a,{a,{a,{a,{a,{a}}}}}}}}}.

-spec f() -> ok.
f() ->
  P = p(),
  case mk_big_map() of
    #{P := _} -> x; % Make the pattern itself so complex Dialyzer *over* simplifies it
    #{{a,{a,{a,{a,{a,{a,{a,{a,{b}}}}}}}}} := _} -> x; % Make the pattern itself so complex Dialyzer *over* simplifies it
    #{one := _} -> ok % With an oversimplified pattern above, Dialyzer erroneously thinks this case can't happen
  end.
