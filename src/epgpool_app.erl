-module(epgpool_app).
-author('Hisham Ismail <mhishami@gmail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    epgpool_sup:start_link().

stop(_State) ->
    ok.
