-module(spop_parse).

-export([playlists/1, playlist_info/1, playlist/1, track/1,
         status/1, offline_status/1, offline_toggle/1,
         uinfo/1, add/1, query_response/1, image/1]).

-include("spop.hrl").

%% Data Parsing ---------------------------------------------------------------

playlists({error, Error}) ->
    {error, Error};
playlists({obj, [{"error", Error}]}) ->
    {error, Error};
playlists({obj, [{"playlists", Playlists}]}) ->
    lists:flatten([ playlist_info(P) || {obj, P} <- Playlists ]).

playlist_info({error, Error}) ->
    {error, Error};
playlist_info({obj, [{"error", Error}]}) ->
    {error, Error};
playlist_info(Pl) ->
    case proplists:get_value("type", Pl, <<"playlist">>) of
        <<"playlist">> = Type ->
            Index = proplists:get_value("index", Pl),
            Name = proplists:get_value("name", Pl),
            NumTracks = proplists:get_value("tracks", Pl),
            Offline = proplists:get_value("offline", Pl),
            #playlist_info{index=Index, name=Name, type=Type,
                           num_tracks=NumTracks, offline=Offline};
        <<"folder">> ->
            Playlists = proplists:get_value("playlists", Pl),
            [ playlist_info(P) || {obj, P} <- Playlists ]
    end.

playlist({error, Error}) ->
    {error, Error};
playlist({obj, [{"error", Error}]}) ->
    {error, Error};
playlist({obj, Obj}) ->
    Tracks = proplists:get_value("tracks", Obj),
    #playlist{name = proplists:get_value("name", Obj),
              offline = proplists:get_value("offline", Obj),
              tracks = [ track(T) || T <- Tracks ]}.

track({error, Error}) ->
    {error, Error};
track({obj, [{"error", Error}]}) ->
    {error, Error};
track({obj, Obj}) ->
    #track{artist = proplists:get_value("artist", Obj),
           title = proplists:get_value("title", Obj),
           album = proplists:get_value("album", Obj),
           duration = proplists:get_value("duration", Obj),
           uri = proplists:get_value("uri", Obj),
           available = proplists:get_value("available", Obj),
           popularity = proplists:get_value("popularity", Obj),
           index = proplists:get_value("index", Obj)}.

status({error, Error}) ->
    {error, Error};
status(<<>>) ->
    {error, empty};
status({obj, [{"error", Error}]}) ->
    {error, Error};
status({obj, Obj}) ->
    #status{status = proplists:get_value("status", Obj),
            repeat = proplists:get_value("repeat", Obj),
            shuffle = proplists:get_value("shuffle", Obj),
            total_tracks = proplists:get_value("total_tracks", Obj),
            current_track = proplists:get_value("current_track", Obj),
            artist = proplists:get_value("artist", Obj),
            title = proplists:get_value("title", Obj),
            album = proplists:get_value("album", Obj),
            duration = proplists:get_value("duration", Obj),
            position = proplists:get_value("position", Obj),
            uri = proplists:get_value("uri", Obj),
            popularity = proplists:get_value("popularity", Obj)}.

offline_status({error, Error}) ->
    {error, Error};
offline_status({obj, [{"error", Error}]}) ->
    {error, Error};
offline_status({obj, Obj}) ->
    #offline_status{offline_playlists =
                        proplists:get_value("offline_playlists", Obj),
                    tracks_to_sync =
                        proplists:get_value("tracks_to_sync", Obj),
                    sync_in_progress =
                        proplists:get_value("sync_in_progress", Obj),
                    time_before_relogin =
                        proplists:get_value("time_before_relogin", Obj)}.

offline_toggle({error, Error}) ->
    {error, Error};
offline_toggle({obj, [{"error", Error}]}) ->
    {error, Error};
offline_toggle({obj, [{"offline", Status}]}) ->
    {offline, Status}.

uinfo({error, Error}) ->
    {error, Error};
uinfo({obj, [{"error", Error}]}) ->
    {error, Error};
uinfo({obj, Obj}) ->
    #uri_info{type = proplists:get_value("type", Obj),
              artist = proplists:get_value("artist", Obj),
              title = proplists:get_value("title", Obj),
              album = proplists:get_value("album", Obj),
              duration = proplists:get_value("duration", Obj),
              offset = proplists:get_value("offset", Obj),
              available = proplists:get_value("available", Obj),
              popularity = proplists:get_value("popularity", Obj)}.

add({error, Error}) ->
    {error, Error};
add({obj, [{"error", Error}]}) ->
    {error, Error};
add({obj, [{"total_tracks", Total}]}) ->
    {total_tracks, Total}.

query_response({error, Error}) ->
    {error, Error};
query_response({obj, [{"error", Error}]}) ->
    {error, Error};
query_response({obj, Obj}) ->
    Tracks = proplists:get_value("tracks", Obj),
    Albums = proplists:get_value("albums", Obj),
    Artists = proplists:get_value("artists", Obj),
    Playlists = proplists:get_value("playlists", Obj),
    #query_response{query_asked = proplists:get_value("query", Obj),
                    uri = proplists:get_value("uri", Obj),
                    total_tracks = proplists:get_value("total_tracks", Obj),
                    total_albums = proplists:get_value("total_albums", Obj),
                    total_artists = proplists:get_value("total_artists", Obj),
                    total_playlists =
                        proplists:get_value("total_playlists", Obj),
                    tracks = [ track(T) || T <- Tracks ],
                    albums = [ query_album(A) || A <- Albums ],
                    artists = [ query_artist(A) || A <- Artists ],
                    playlists = [ query_playlist(P) || P <- Playlists ]}.

query_artist({obj, Obj}) ->
    #artist{name = proplists:get_value("artist", Obj),
            uri = proplists:get_value("uri", Obj)}.

query_album({obj, Obj}) ->
    #album{artist = proplists:get_value("artist", Obj),
           title = proplists:get_value("title", Obj),
           available = proplists:get_value("available", Obj),
           uri = proplists:get_value("uri", Obj)}.

query_playlist({obj, Obj}) ->
    #playlist_ref{name = proplists:get_value("name", Obj),
                  uri = proplists:get_value("uri", Obj)}.

image({error, Error}) ->
    {error, Error};
image({obj, [{"error", Error}]}) ->
    {error, Error};
image({obj, Obj}) ->
    {jpeg, base64:decode(proplists:get_value("data", Obj, <<>>))}.
