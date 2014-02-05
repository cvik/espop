%% Record definitions for espop
%%
%% ----------------------------------------------------------------------------


-copyright("Christoffer Vikström <chvi77@gmail.com>").

-record(playlist_info, {index, name, type, num_tracks, offline}).

-record(status, {status, repeat, shuffle, total_tracks, current_track,
                 artist, title, album, duration, position, uri, popularity}).

-record(track, {artist, title, album, duration, uri,
                available, popularity, index}).

-record(playlist, {name, offline, tracks=[]}).

-record(uri_info, {type, artist, title, album, duration,
                   offset, available, popularity}).

-record(offline_status, {offline_playlists, tracks_to_sync,
                         sync_in_progress, time_before_relogin}).

-record(album, {title, artist, available, uri}).

-record(artist, {name, uri}).

-record(query_response, {query_asked, uri,
                         total_tracks, total_albums,
                         total_artists, total_playlists,
                         tracks, albums, artists, playlists}).

-record(playlist_ref, {name, uri}).
