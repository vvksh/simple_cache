%%%-------------------------------------------------------------------
%%% @author viveks
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2021 11:22 PM
%%%-------------------------------------------------------------------
-module(sc_element).
-author("viveks").
-behavior(gen_server).


%% API
-export([init/1, handle_call/3, handle_cast/2]).
-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60*60*24)).
-record(state, {value, lease_time, start_time}).


%%%-----------------------------
%%% When a element is to inserted, users call sc_element_create(...)
%%% This calls the supervisor's sc_sup:start_child function
%%% which then calls sc_element:start_link/2 function
%%% Note that we dont have a registered name for sc_element
%%% as there can be so many of them, so user needs to know the Pid of the
%%% element, will address later
%%%-----------------------------

start_link(Value, LeaseTime) ->
  gen_server:start(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
  sc_sup:start_child(Value, LeaseTime).

create(Value) ->
  create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
  gen_server:call(Pid, fetch).

replace(Pid, Value) ->
  gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
  gen_server:cast(Pid, delete).

init([Value, LeaseTime]) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  {ok,
    #state{value = Value,
          lease_time = LeaseTime,
          start_time = StartTime},
    time_left(StartTime, LeaseTime)}. %% set timeout

handle_call(fetch, From, State) ->
  % Deconstruct the state to get values and update timeout
  #state{value=Value,
        lease_time = LeaseTime,
        start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
  #state{lease_time = LeaseTime,
    start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {noreply, State#state{value=Value}, TimeLeft};

handle_cast(delete, State) ->
  {stop, normal, State}.

handle_info(timeout, State) ->
  {stop, normal, State}.

terminate(Reason, State) ->
  ok.

time_left(StartTime, infinity) ->
  infinity;

time_left(StartTime, LeaseTime) ->
  Now = calendar:local_time(),
  CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
  TimeElapsed = CurrentTime - StartTime,
  case LeaseTime - TimeElapsed of
    Time when Time =< 0 -> 0;
    Time                -> Time * 1000
  end.