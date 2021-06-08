%%%-------------------------------------------------------------------
%%% @author viveks
%%% @copyright (C) 2021, <COMPANY>
%%% @doc This module implements mapping between key and process ids which hold the values.
%%%      Uses ETS which is an in-memory hash table to store this mapping.
%%%      This module is supposed to abstract the ETS storage such that in future, we could
%%%      move to a different storage without changing much of application code.
%%% @end
%%% Created : 07. Jun 2021 11:57 PM
%%%-------------------------------------------------------------------
-module(sc_store).
-author("viveks").

%% API
-export([init/0, insert/2, lookup/1, delete/1]).

-define(TABLE_ID, ?MODULE).

init() ->
  % marked public so that other processes can access this table
  % named_table makes it easy to find by name which here is TABLE_ID
  % Sidenote: without named_table, you'd have to hold on to the table
  % handle returned by the ets:new function
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Key, Pid) ->
  ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] -> {ok, Pid};
    []           -> {error, not_found}
  end.

delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).