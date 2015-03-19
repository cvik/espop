%%  Server that handle asynchronous events from espopd
%%
%% ----------------------------------------------------------------------------

-module(espop_event).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

-export([start_link/2]).

-export([watch/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {spop_version, socket, host, port}).

%% Management Api -------------------------------------------------------------

start_link(Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

%% Api ------------------------------------------------------------------------

watch(Pid) ->
    gen_server:cast(?MODULE, {watch, Pid}).

%% gen_server callbacks -------------------------------------------------------

init([Host, Port]) ->
    case connect(Host, Port) of
        {ok, {S, Version}} ->
            {ok, #state{spop_version=Version, socket=S, host=Host, port=Port}};
        {error, Error} ->
            {stop, Error}
    end.

handle_call(Msg, _, State) ->
    error_logger:warning_msg("unexpected: ~p", [Msg]),
    {noreply, State}.

handle_cast({watch, Pid}, #state{socket=S, host=Host, port=Port} = State) ->
    case gen_tcp:send(S, <<"idle", 10>>) of
        ok ->
            Status = espop_util:recv_lines(S, 1),
            Pid ! {espop_event, espop_parse:status(Status)},
            watch(Pid),
            {noreply, State};
        {error, closed} ->
            case connect(Host, Port) of
                {ok, {NewS, Version}} ->
                    watch(Pid),
                    {noreply, State#state{socket=NewS, spop_version=Version}};
                {error, Reason} ->
                    Pid ! {espop_error, Reason},
                    {noreply, State}
            end;
        {error, Reason} ->
            Pid ! {espop_error, Reason},
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

%% Internal -------------------------------------------------------------------

connect(Host, Port) ->
    Ops = [binary, {packet, 0}, {active, false}],
    case gen_tcp:connect(Host, Port, Ops) of
        {ok, S} ->
            {ok, Version} = gen_tcp:recv(S, 0),
            {ok, {S, Version}};
        {error, Error} ->
            {error, Error}
    end.
