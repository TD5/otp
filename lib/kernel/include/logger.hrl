-ifndef(LOGGER_HRL).
-define(LOGGER_HRL,true).
-define(LOG_EMERGENCY(A),?DO_LOG1(emergency,A)).
-define(LOG_EMERGENCY(A,B),?DO_LOG2(emergency,A,B)).
-define(LOG_EMERGENCY(A,B,C),?DO_LOG3(emergency,A,B,C)).

-define(LOG_ALERT(A),?DO_LOG1(alert,A)).
-define(LOG_ALERT(A,B),?DO_LOG2(alert,A,B)).
-define(LOG_ALERT(A,B,C),?DO_LOG3(alert,A,B,C)).

-define(LOG_CRITICAL(A),?DO_LOG1(critical,A)).
-define(LOG_CRITICAL(A,B),?DO_LOG2(critical,A,B)).
-define(LOG_CRITICAL(A,B,C),?DO_LOG3(critical,A,B,C)).

-define(LOG_ERROR(A),?DO_LOG1(error,A)).
-define(LOG_ERROR(A,B),?DO_LOG2(error,A,B)).
-define(LOG_ERROR(A,B,C),?DO_LOG3(error,A,B,C)).

-define(LOG_WARNING(A),?DO_LOG1(warning,A)).
-define(LOG_WARNING(A,B),?DO_LOG2(warning,A,B)).
-define(LOG_WARNING(A,B,C),?DO_LOG3(warning,A,B,C)).

-define(LOG_NOTICE(A),?DO_LOG1(notice,A)).
-define(LOG_NOTICE(A,B),?DO_LOG2(notice,A,B)).
-define(LOG_NOTICE(A,B,C),?DO_LOG3(notice,A,B,C)).

-define(LOG_INFO(A),?DO_LOG1(info,A)).
-define(LOG_INFO(A,B),?DO_LOG2(info,A,B)).
-define(LOG_INFO(A,B,C),?DO_LOG3(info,A,B,C)).

-define(LOG_DEBUG(A),?DO_LOG1(debug,A)).
-define(LOG_DEBUG(A,B),?DO_LOG2(debug,A,B)).
-define(LOG_DEBUG(A,B,C),?DO_LOG3(debug,A,B,C)).

-define(LOG(L,A),?DO_LOG1(L,A)).
-define(LOG(L,A,B),?DO_LOG2(L,A,B)).
-define(LOG(L,A,B,C),?DO_LOG3(L,A,B,C)).

-define(LOCATION,#{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
                   line=>?LINE,
                   file=>?FILE}).

%%%-----------------------------------------------------------------
%%% Internal, i.e. not intended for direct use in code - use above
%%% macros instead!
-define(DO_LOG1(Level,A),
        case logger:allow(Level,?MODULE) of
            true ->
                logger:macro_log(?LOCATION,Level,(A));
            false ->
                ok
        end).
-define(DO_LOG2(Level,A,B),
        case logger:allow(Level,?MODULE) of
            true ->
                logger:macro_log(?LOCATION,Level,(A),(B));
            false ->
                ok
        end).
-define(DO_LOG3(Level,A,B,C),
        case logger:allow(Level,?MODULE) of
            true ->
                logger:macro_log(?LOCATION,Level,(A),(B),(C));
            false ->
                ok
        end).
-endif.
