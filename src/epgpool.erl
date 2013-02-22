-module (epgpool).
-author('Hisham Ismail <mhishami@gmail.com').

-export ([start/0]).
-export ([stop/0]).
-export ([squery/2, equery/3]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
	ok = ensure_started(crypto),
    ok = ensure_started(poolboy),
    ok = ensure_started(epgpool).
    
stop() ->
    application:stop(epgpool),
    ok.

squery(PoolName, Sql) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
    end).

equery(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
    end).
