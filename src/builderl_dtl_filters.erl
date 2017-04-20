-module(builderl_dtl_filters).

-behaviour(erlydtl_library).

-export([version/0,
         inventory/1]).

-export([sToDatetime/1]).

version() ->
  1.

inventory(filters) ->
  io:format("\n\nFiltering\n\n"),
  [sToDatetime];
inventory(tags) ->
  io:format("\nTagging\n"),
  [].


sToDatetime(Arg) ->
  io:format("huh? ~p~n", [Arg]),
  ["foob"].
