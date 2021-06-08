%%%-------------------------------------------------------------------
%%% @author Vivek Sah
%%% @doc This is an application-level api module which provides
%%%      api to the simple_cache application
%%% @end
%%% Created : 08. Jun 2021 12:34 AM
%%%-------------------------------------------------------------------
-module(simple_cache).
-author("viveks").

%% API
-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
  case sc_store:lookup(Key) of
    {ok, Pid} -> sc_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = sc_element:create(Value),
      sc_store:insert(Key, Pid)
  end.

lookup(Key) ->
  try
     {ok, Pid} = sc_store:lookup(Key),
     {ok, Value} = sc_element:fetch(Pid),
     {ok, Value}
  catch
      _Class:_Exception  -> {error, not_found}
  end.

delete(Key) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.