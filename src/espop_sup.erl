%%  Top supervisor for espop
%%
%% ----------------------------------------------------------------------------

-module(espop_sup).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-export([start_link/0]).

-export([init/1]).

-behaviour(supervisor).

%% ----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% ----------------------------------------------------------------------------

init(no_arg) ->
    Env = application:get_all_env(spop),
    Host = proplists:get_value(host, Env, localhost),
    Port = proplists:get_value(host, Env, 6602),
    Srv = child(espop_event, espop_event, worker, [Host, Port]),
    Strategy = {one_for_one, 1, 60},
    {ok, {Strategy, [Srv]}}.

%% ----------------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, permanent, 3000, Type, [Mod]}.
