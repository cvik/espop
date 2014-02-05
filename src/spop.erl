-module(spop).

-export([start/0, stop_app/0]).

-export([ls/0, ls/1, qls/0, qclear/0, qrm/1, add/1, add/2, play/1, play/2,
         uinfo/1, uadd/1, uplay/1, search/1, play/0, toggle/0, stop/0,
         seek/1, next/0, prev/0, goto/1, repeat/0, shuffle/0, status/0,
         image/0, offline_status/0, offline_toggle/1, watch_events/0, quit/0]).

-export([recv_all/3]).

-include("spop.hrl").

%% Management Spi -------------------------------------------------------------

start() ->
    application:start(spop).

stop_app() ->
    application:stop(spop).

%% Api ------------------------------------------------------------------------

ls() ->
    spop_parse:playlists(send(ls, [])).

ls(PlaylistNum) ->
    spop_parse:playlist(send(ls, [PlaylistNum])).

qls() ->
    spop_parse:playlist(send(qls, [])).

qclear() ->
    spop_parse:status(send(qclear, [])).

qrm(TrackNum) ->
    spop_parse:status(send(qrm, [TrackNum])).

qrm(FromTrackNum, ToTrackNum) ->
    spop_parse:status(send(qrm, [FromTrackNum, ToTrackNum])).

add(PlaylistNum) ->
    spop_parse:add(send(add, [PlaylistNum])).

add(PlaylistNum, TrackNum) ->
    spop_parse:add(send(add, [PlaylistNum, TrackNum])).

play(PlaylistNum) ->
    spop_parse:status(send(play, [PlaylistNum])).

play(PlaylistNum, TrackNum) ->
    spop_parse:status(send(play, [PlaylistNum, TrackNum])).

uinfo(Uri) ->
    spop_parse:uinfo(send(uinfo, [Uri])).

uadd(Uri) ->
    spop_parse:status(send(uadd, [Uri])).

uplay(Uri) ->
    spop_parse:status(send(uplay, [Uri])).

search(Query) ->
    spop_parse:query_response(send(search, [Query])).

play() ->
    spop_parse:status(send(play, [])).

toggle() ->
    spop_parse:status(send(toggle, [])).

stop() ->
    spop_parse:status(send(stop, [])).

seek(PosMilliSec) ->
    spop_parse:status(send(seek, [PosMilliSec])).

next() ->
    spop_parse:status(send(next, [])).

prev() ->
    spop_parse:status(send(prev, [])).

goto(TrackNum) ->
    spop_parse:status(send(goto, [TrackNum])).

repeat() ->
    spop_parse:status(send(repeat, [])).

shuffle() ->
    spop_parse:status(send(shuffle, [])).

status() ->
    spop_parse:status(send(status, [])).

image() ->
    send(image, []).

offline_status() ->
    spop_parse:offline_status(send('offline-status', [])).

offline_toggle(PlaylistNum) ->
    spop_parse:offline_toggle(send('offline-toggle', [PlaylistNum])).

watch_events() ->
    spop_event:watch().

quit() ->
    send(quit, []),
    ok.

%% Internal -------------------------------------------------------------------

send(Cmd, Args) ->
    Env = application:get_all_env(spop),
    Host = proplists:get_value(host, Env, localhost),
    Port = proplists:get_value(port, Env, 6602),
    send(Cmd, Args, Host, Port).

send(Cmd, Args, Host, Port) ->
    Ops = [binary, {packet, 0}, {active, false}],
    case gen_tcp:connect(Host, Port, Ops) of
        {ok, S} ->
            {ok, _Version} = gen_tcp:recv(S, 0),
            CmdBin = pack_command(Cmd, Args),
            ok = gen_tcp:send(S, <<CmdBin/binary, 10>>),
            Data = recv_all(S, 1, []),
            gen_tcp:close(S),
            Data;
        {error, Error} ->
            {error, Error}
    end.

pack_command(Cmd, Args) ->
    Str = string:join([to_string(Cmd) | [ to_string(A) || A <- Args ] ], " "),
    list_to_binary(Str).

to_string(T) when is_integer(T) -> integer_to_list(T);
to_string(T) when is_float(T) -> float_to_list(T);
to_string(T) when is_atom(T) -> atom_to_list(T);
to_string(T) when is_binary(T) -> binary_to_list(T);
to_string(T) when is_list(T) -> "\""++T++"\"".

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
