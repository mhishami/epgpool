-module(epgpool_sup).
-author('Hisham Ismail <mhishami@gmail.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    
    {ok, PGPools} = application:get_env(epgpool, pools),    
    Fun = fun({Name, SizeArgs, WorkerArgs}) ->
              PoolArgs = [
                  {name, {local, Name}},
                  {worker_module, epgpool_srv}
              ] ++ SizeArgs,
              poolboy:child_spec(Name, PoolArgs, WorkerArgs)
          end,
    Specs = lists:map(Fun, PGPools),
    
    {ok, { {one_for_one, 5, 10}, Specs} }.

