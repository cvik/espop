%%  Application callback for spop
%%
%% ----------------------------------------------------------------------------

-module(spop_app).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-export([start/2, prep_stop/1, stop/1]).

-behaviour(application).

%% ----------------------------------------------------------------------------

start(_, _) ->
    spop_sup:start_link().

prep_stop(State) ->
    State.

stop(_) ->
    ok.

%% ----------------------------------------------------------------------------
