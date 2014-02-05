espop - Erlang client library for spop
=====

This is a client library written in erlang ment to interface with
the excellent spop server http://github.com/Schnouki/spop. It support
all current commands of the spopd server. See the facade, espop.erl, for
documentation and specs.

## Installation

- Use rebar (not included) to build
- There are no dependencies to download

## Example usage

```erlang
1> espop:start().
ok
2> rr(espop).
[album,artist,offline_status,playlist,playlist_info,
 playlist_ref,query_response,status,track,uri_info]
3> espop:play(1)
#status{status = <<"playing">>,repeat = false,
         shuffle = false,total_tracks = 19,current_track = 1,
         artist = <<"L'Orchestra Cinematique">>,
         title = <<"The King's Arrival (from Game of Thrones - Season 1)">>,
         album = <<"Game of Thrones - Best of Seasons 1, 2 & 3">>,
         duration = 216000,position = 0.371,
         uri = <<"spotify:track:4tJqf2Cc9JP77Z984hJdYl">>,
         popularity = 45}
```

For the above to work, the spopd server must be running at localhost:6602.

Set the application environment variables 'host' and 'port' to change which
server to talk to.

## License

Apache license version 2.0. See the LICENSE file for details.

The file src/rfc4627.erl is excluded from this license and is
borrowed from http://github.com/tonyg/erlang-rfc4627. See this file
for copyright and license details.
