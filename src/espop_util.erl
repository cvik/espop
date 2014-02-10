%% Utilities
%%
%% ----------------------------------------------------------------------------

-module(espop_util).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-export([recv_lines/2]).

recv_lines(Socket, NumLines) ->
    recv_lines(Socket, NumLines, []).

recv_lines(_, 0, Acc) ->
    {ok, Json, []} = rfc4627:decode(iolist_to_binary(lists:reverse(Acc))),
    Json;
recv_lines(S, Cnt, Acc) ->
    case gen_tcp:recv(S, 0) of
        {ok, Data} ->
            case binary:match(Data, <<"\n">>) of
                nomatch ->
                    recv_lines(S, Cnt, [Data|Acc]);
                _ ->
                    recv_lines(S, Cnt-1, [Data|Acc])
            end;
        {error, _} ->
            iolist_to_binary(lists:reverse(Acc))
    end.
