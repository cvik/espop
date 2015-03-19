%% Client API to talk to a spopd server
%%
%% ----------------------------------------------------------------------------

-module(espop).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

%% Aanagement Api
-export([start/0, stop_app/0]).

%% Api
-export([ls/0, ls/1, qls/0, qclear/0, qrm/1, qrm/2, add/1, add/2, play/1,
         play/2, uinfo/1, uadd/1, uplay/1, search/1, play/0, toggle/0, stop/0,
         seek/1, next/0, prev/0, goto/1, repeat/0, shuffle/0, status/0,
         image/0, offline_status/0, offline_toggle/1, watch_events/1, quit/0]).

%% Utillity
-export([find/1]).

-include("espop.hrl").

%% Management Api -------------------------------------------------------------

start() ->
    application:start(espop).

stop_app() ->
    application:stop(espop).

%% Api ------------------------------------------------------------------------

%% @doc list all your playlists
-spec ls() -> [#playlist_info{}] | {error, atom()}.
ls() ->
    espop_parse:playlists(send(ls, [])).

%% @doc list the contents of playlist number PlayListNum
-spec ls(PlayListNum :: integer()) -> [#track{}] | {error, atom()}.
ls(PlaylistNum) ->
    espop_parse:playlist(send(ls, [PlaylistNum])).

%% @doc list the contents of the queue
-spec qls() -> #playlist{} | {error, atom()}.
qls() ->
    espop_parse:playlist(send(qls, [])).

%% @doc clear the contents of the queue
-spec qclear() -> #status{} | {error, atom()}.
qclear() ->
    espop_parse:status(send(qclear, [])).

%% @doc remove track number TrackNum from the queue
-spec qrm(TrackNum :: integer()) -> #status{} | {error, atom()}.
qrm(TrackNum) ->
    espop_parse:status(send(qrm, [TrackNum])).

%% @doc remove tracks FromTrackNum to ToTrackNum from the queue
-spec qrm(FromTrackNum :: integer(), ToTrackNum :: integer()) ->
        #status{} | {error, atom()}.
qrm(FromTrackNum, ToTrackNum) ->
    espop_parse:status(send(qrm, [FromTrackNum, ToTrackNum])).

%% @doc add playlist number PlaylistNum to the queue
-spec add(PlaylistNum :: integer()) ->
        {total_tracks, integer()} | {error, atom()}.
add(PlaylistNum) ->
    espop_parse:add(send(add, [PlaylistNum])).

%% @doc  add track number TrackNum from playlist number PlaylistNum to the queue
-spec add(PlaylistNum :: integer(), TrackNum :: integer()) ->
        {total_trcks, integer()} | {error, atom()}.
add(PlaylistNum, TrackNum) ->
    espop_parse:add(send(add, [PlaylistNum, TrackNum])).

%% @doc  replace the contents of the queue with playlist PlaylistNum
%%       and start playing
-spec play(PlaylistNum :: integer()) -> #status{} | {error, atom()}.
play(PlaylistNum) ->
    espop_parse:status(send(play, [PlaylistNum])).

%% @doc replace the contents of the queue with track TrackNum
%%      from playlist PlaylistNum and start playing
-spec play(PlaylistNum :: integer(), TrackNum :: integer()) ->
        #status{} | {error, atom()}.
play(PlaylistNum, TrackNum) ->
    espop_parse:status(send(play, [PlaylistNum, TrackNum])).

%% @doc display information about the given Spotify URI
-spec uinfo(Uri :: binary()) -> #uri_info{} | {error, atom()}.
uinfo(Uri) ->
    espop_parse:uinfo(send(uinfo, [Uri])).

%% @doc add the given Spotify URI to the queue (playlist, track or album only)
-spec uadd(Uri :: binary()) -> {total_tracks, integer()} | {error, atom()}.
uadd(Uri) ->
    espop_parse:status(send(uadd, [Uri])).

%% @doc replace the contents of the queue with the given Spotify URI
%%      (playlist, track or album only) and start playing
-spec uplay(Uri :: binary()) -> #status{} | {error, atom()}.
uplay(Uri) ->
    espop_parse:status(send(uplay, [Uri])).

%% @doc perform a search with the given query
-spec search(Query :: string()) -> #query_response{} | {error, atom()}.
search(Query) ->
    espop_parse:query_response(send(search, [Query])).

%% @doc start playing from the queue
-spec play() -> #status{} | {error, atom()}.
play() ->
    espop_parse:status(send(play, [])).

%% @doc toggle pause mode
-spec toggle() -> #status{} | {error, atom()}.
toggle() ->
    espop_parse:status(send(toggle, [])).

%% @doc stop playback
-spec stop() -> #status{} | {error, atom()}.
stop() ->
    espop_parse:status(send(stop, [])).

%% @doc go to position PosMilliSec in the current track
-spec seek(PosMilliSec :: integer()) -> #status{} | {error, atom()}.
seek(PosMilliSec) ->
    espop_parse:status(send(seek, [PosMilliSec])).

%% @doc switch to the next track in the queue
-spec next() -> #status{} | {error, atom()}.
next() ->
    espop_parse:status(send(next, [])).

%% @doc switch to the previous track in the queue
-spec prev() -> #status{} | {error, atom()}.
prev() ->
    espop_parse:status(send(prev, [])).

%% @doc switch to track number TrackNum in the queue
-spec goto(TrackNum :: integer()) -> #status{} | {error, atom()}.
goto(TrackNum) ->
    espop_parse:status(send(goto, [TrackNum])).

%% @doc toggle repeat mode
-spec repeat() -> #status{} | {error, atom()}.
repeat() ->
    espop_parse:status(send(repeat, [])).

%% @doc toggle shuffle mode
-spec shuffle() -> #status{} | {error, atom()}.
shuffle() ->
    espop_parse:status(send(shuffle, [])).

%% @doc  display informations about the queue, the current track, etc.
-spec status() -> #status{} | {error, atom()}.
status() ->
    espop_parse:status(send(status, [])).

%% @doc get the cover image for the current track (jpeg binary)
-spec image() -> {jpeg, binary()} | {error, atom()}.
image() ->
    espop_parse:image(send(image, [])).

%% @doc display informations about the current status of the offline cache
%%      (number of offline playlists, sync status..)
-spec offline_status() -> #offline_status{} | {error, atom()}.
offline_status() ->
    espop_parse:offline_status(send('offline-status', [])).

%% @doc toggle offline mode for playlist number PlayListNum
-spec offline_toggle(PlaylistNum :: integer()) ->
        {offline, true|false} | {error, atom()}.
offline_toggle(PlaylistNum) ->
    espop_parse:offline_toggle(send('offline-toggle', [PlaylistNum])).

%% @doc receive spopd events (status changes) as events sent to the pid
%%      calling this function. Currently only works for one pid at a time.
%%      sends {espop_event, #status{}} messages.
-spec watch_events(Pid :: pid()) -> ok.
watch_events(Pid) ->
    espop_event:watch(Pid).

%% @doc Shut down the spopd server
-spec quit() -> ok.
quit() ->
    send(quit, []),
    ok.

%% Utillity -------------------------------------------------------------------

%% @doc Prints all playlists, with index, that match the substring SearchString
-spec find(string()) -> ok.
find(SearchString) ->
    Playlists = ls(),
    Test = fun(_, <<>>) -> true;
              (Src, Sub) -> binary:match(Src, Sub) /= nomatch
           end,
    [ io:format("~p - ~ts~n", [I, N]) ||
      #playlist_info{index=I, name=N} <- Playlists,
      Test(to_lower(N), to_lower(SearchString)) ],
    ok.

to_lower(T) when is_list(T) ->
    list_to_binary(string:to_lower(T));
to_lower(T) when is_binary(T) ->
    list_to_binary(string:to_lower(binary_to_list(T))).

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
            Data = espop_util:recv_lines(S, 1),
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
