-module(spop).

-export([start_link/0, start_link/2]).

-compile(export_all).

-record(state, {spop_version, socket}).

-define(HOST, localhost).
-define(PORT, 6602).

%% Management Api -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [localhost, 6602], []).

start_link(Server, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Server, Port], []).
    
%% Api ------------------------------------------------------------------------

ls() ->
    send(ls, []).

ls(PlaylistNum) ->
    send(ls, [PlaylistNum]).

qls() ->
    send(qls, []).

qclear() ->
    send(qclear, []).

qrm(TrackNum) ->
    send(qrm, [TrackNum]).

qrm(FromTrackNum, ToTrackNum) ->
    send(qrm, [FromTrackNum, ToTrackNum]).

add(PlaylistNum) ->
    send(add, [PlaylistNum]).

add(PlaylistNum, TrackNum) ->
    send(add, [PlaylistNum, TrackNum]).

play(PlaylistNum) ->
    send(play, [PlaylistNum]).

play(PlaylistNum, TrackNum) ->
    send(play, [PlaylistNum, TrackNum]).

uinfo(Uri) ->
    send(uinfo, [Uri]).

uadd(Uri) ->
    send(uadd, [Uri]).

uplay(Uri) ->
    send(uplay, [Uri]).

search(Query) ->
    send(search, [Query]).

play() ->
    send(play, []).

toggle() ->
    send(toggle, []).

stop() ->
    send(stop, []).

seek(PosMilliSec) ->
    send(seek, [PosMilliSec]).

next() ->
    send(next, []).

prev() ->
    send(prev, []).

goto(TrackNum) ->
    send(goto, [TrackNum]).

repeat() ->
    send(repeat, []).

shuffle() ->
    send(shuffle, []).

status() ->
    send(status, []).

image() ->
    send(image, []).

offline_status() ->
    send(offline_status, []).

offline_toggle() ->
    send(offline_toggle, []).

offline_toggle(PlaylistNum) ->
    send(offline_toggle, [PlaylistNum]).

bye() ->
    send(bye, []).

exit() ->
    send(exit, []).

watch() ->
    gen_server:cast(?MODULE, {watch, self()}).

%% gen_server callbacks -------------------------------------------------------

init([Server, Port]) ->
    Ops = [binary, {packet, 0}, {active, false}],
    {ok, S} = gen_tcp:connect(Server, Port, Ops), 
    {ok, Version} = gen_tcp:recv(S, 0),
    {ok, #state{spop_version=Version, socket=S}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({watch, Pid}, #state{socket=S} = State) ->
    gen_tcp:send(S, <<"idle", 10>>),
    Response = recv_all(S, 1, []),
    Pid ! {spop_event, Response},
    gen_server:cast(?MODULE, {watch, Pid}),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.
    
terminate(_, _) ->
    ok.

code_change(_, State) ->
    State.

%% Internal -------------------------------------------------------------------

send(Cmd, Args) ->
    send(Cmd, Args, ?HOST, ?PORT).

send(Cmd, Args, Host, Port) ->
    Ops = [binary, {packet, 0}, {active, false}],
    {ok, S} = gen_tcp:connect(Host, Port, Ops),
    {ok, _Version} = gen_tcp:recv(S, 0),
    CmdBin = pack_command(Cmd, Args),
    ok = gen_tcp:send(S, <<CmdBin/binary, 10>>),
    Data = recv_all(S, 1, []),
    ok = gen_tcp:send(S, <<"bye", 10>>),
    gen_tcp:close(S),
    Data.

pack_command(Cmd, Args) ->
    Str = string:join([to_string(Cmd) | [ to_string(A) || A <- Args ] ], " "),
    list_to_binary(Str).

to_string(T) when is_integer(T) -> integer_to_list(T);
to_string(T) when is_float(T) -> float_to_list(T);
to_string(T) when is_atom(T) -> atom_to_list(T);
to_string(T) when is_binary(T) -> binary_to_list(T).

recv_all(_, 0, Acc) ->
    {ok, Json, []} = rfc4627:decode(iolist_to_binary(lists:reverse(Acc))),
    Json;
recv_all(S, Cnt, Acc) ->
    case gen_tcp:recv(S, 0) of
        {ok, Data} ->
            case binary:match(Data, <<"\n">>) of
                nomatch ->
                    recv_all(S, Cnt, [Data|Acc]);
                _ ->
                    recv_all(S, Cnt-1, [Data|Acc])
            end;    
        {error, _} ->
            iolist_to_binary(lists:reverse(Acc))
    end.

%% Data Parsing ----------------------------------------------------------------

-record(playlist, {index, name, type, num_tracks, offline}).

parse_list_output({obj, [{"playlists", Playlists}]}) ->
    lists:flatten([ parse_playlist(P) || {obj, P} <- Playlists ]).

parse_playlist(Pl) ->
    case proplists:get_value("type", Pl) of
        <<"playlist">> = Type ->
            Index = proplists:get_value("index", Pl),
            Name = proplists:get_value("name", Pl),
            NumTracks = proplists:get_value("tracks", Pl),
            Offline = proplists:get_value("offline", Pl),
            #playlist{index=Index, name=Name, type=Type,
                      num_tracks=NumTracks, offline=Offline};
        <<"folder">> ->
            Playlists = proplists:get_value("playlists", Pl),
            [ parse_playlist(P) || {obj, P} <- Playlists ]
    end.
