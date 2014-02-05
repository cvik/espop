%%  Server that handle asynchronous events from espopd
%%
%% ----------------------------------------------------------------------------

-module(espop_event).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

-export([start_link/2]).

-export([watch/0]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {spop_version, socket, host, port}).

%% Management Api -------------------------------------------------------------

start_link(Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

%% Api ------------------------------------------------------------------------

watch() ->
    gen_server:cast(?MODULE, {watch, self()}).

%% gen_server callbacks -------------------------------------------------------

init([Host, Port]) ->
    Ops = [binary, {packet, 0}, {active, false}],
    case gen_tcp:connect(Host, Port, Ops) of
        {ok, S} ->
            {ok, Version} = gen_tcp:recv(S, 0),
            {ok, #state{spop_version=Version, socket=S, host=Host, port=Port}};
        {error, Error} ->
            {stop, Error}
    end.

handle_call(Msg, _, State) ->
    error_logger:warning_msg("unexpected: ~p", [Msg]),
    {noreply, State}.

handle_cast({watch, Pid}, #state{socket=S} = State) ->
    case gen_tcp:send(S, <<"idle", 10>>) of
        ok ->
            Status = espop:recv_all(S, 1, []),
            Pid ! {espop_event, espop_parse:status(Status)},
            gen_server:cast(?MODULE, {watch, Pid}),
            {noreply, State};
        {error, Reason} ->
            Pid ! {espop_event, {error, Reason}},
            {noreply, State}
    end;
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    State.

