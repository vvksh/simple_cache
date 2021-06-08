%%%-------------------------------------------------------------------
%%% @author viveks
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2021 10:42 PM
%%%-------------------------------------------------------------------
-module(sc_sup).
-author("viveks").
-behavior(supervisor).

%% API
-export([start_link/0,
        start_child/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%----------
%%% In this module, we will setup `simple_one_for_one` supervision
%%% which means it can start any number of just one type of child processes.
%%% However, no child process is started when the supervisor starts, but will
%%% dynamically added.

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
  supervisor:start_child(?SERVER, [Value, LeaseTime]).

init([]) ->
  %% TODO implement sc_element

  % declare children which will be of type sc_element
  % whenever start_child/2 is called with [Value and LeaseTime],
  % it will call sc_element:start_link with [Value, LeaseTime] appended to input []
  % see child spec: https://erlang.org/doc/man/supervisor.html#type-child_spec
  % TODO: Use spec map instead of tuple
  ElementSpec = {sc_element, %id
            {sc_element, start_link, []}, % start
            temporary, % restart: don't restart children if died
            brutal_kill, % immediately shut down children
            worker, % type
            [sc_element] }, % modules
  Children = [ElementSpec],

  % 0 restarts allowed within any 1 second
  RestartStrategy = #{strategy => simple_one_for_one,
                      intensity => 0,
                      period => 1},
  {ok, {RestartStrategy, Children}}.