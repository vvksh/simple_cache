%%%-------------------------------------------------------------------
%%% @author viveks
%%% @doc This is the application module which starts/stops the application
%%% @end
%%% Created : 07. Jun 2021 10:42 PM
%%%-------------------------------------------------------------------

-module(sc_app).
-author("viveks").

-behavior(application).

% expected callbacks for application behavior
-export([start/2, stop/1]).

start(StartType, StartArgs) ->
  % we only want to start the supervisor for now
  sc_store:init(),
  case sc_sup:start_link() of
    {ok, Pid} -> {ok, Pid};
    Other     -> {error, Other}
  end.

stop(State) ->
  ok.