-module(epgpool_srv).
-author('Hisham Ismail <mhishami@gmail.com').

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include ("epgpool.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
% -export ([squery/1, equery/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

% squery(Stmt) ->
%     gen_server:call(?SERVER, {squery, Stmt}).
%     
% equery(Stmt, Params) ->
%     gen_server:call(?SERVER, {equery, Stmt, Params}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    process_flag(trap_exit, true),
    
    ?INFO("Args: ~p~n", [Args]),
    
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
        
    {ok, Conn} = pgsql:connect(Hostname, Username, Password, [
        {database, Database}
    ]),
    {ok, #state{conn=Conn}}.

handle_call({squery, Stmt}, _From, #state{conn=Conn} = State) ->
    ?INFO("%% SQUERY: ~p~n", [Stmt]),
    {reply, pgsql:squery(Conn, Stmt), State};

handle_call({equery, Stmt, Params}, _From, #state{conn=Conn} = State) ->
    ?INFO("%% EQUERY: ~p,~n%% [~p]~n", [Stmt, Params]),
    {reply, pgsql:equery(Conn, Stmt, Params), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

