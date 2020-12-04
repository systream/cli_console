%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console_parser).
-author("Peter Tihanyi").

-type args() :: {string(), true | string()}.

%% API
-export([parse/1]).

-spec parse(string() | binary()) ->
  {ok, Command :: list(string()), Arguments :: [args()] | proplists:proplist()}.
parse(Data) when is_list(Data) ->
  Tokens = string:lexemes(Data, " " ++ [$\r, $\n, $\\]),
  {Command, Args} = lists:splitwith(fun is_not_arg/1, Tokens),
  {ok, Command, parse_args(Args)};
parse(Data) when is_binary(Data) ->
  parse(binary_to_list(Data)).

-spec parse_args(list(string())) -> [args()].
parse_args(Flags) ->
  parse_args(Flags, []).

-spec parse_args(list(string()), [args()]) -> [args()].
parse_args([], Acc) ->
  lists:reverse(Acc);
parse_args([[$-, $- | Key] | Rest], Acc) ->
  {Item, Remaining} = maybe_add_value(Key, Rest),
  parse_args(Remaining, [Item | Acc]);
parse_args([[$- | Key] | Rest], Acc) ->
  {Item, Remaining} = maybe_add_value(Key, Rest),
  parse_args(Remaining, [Item | Acc]);
parse_args([Unknown | Rest], Acc) ->
  parse_args(Rest, [Unknown | Acc]).

-spec maybe_add_value(string(), [string()]) -> {args(), [string()]}.
maybe_add_value(Key, [Next | Rest]) ->
  maybe_add_value(Key, Next, Rest, is_arg(Next));
maybe_add_value(Key, Rest) ->
  {equal_sign_parse(Key), Rest}.

-spec maybe_add_value(string(), string(), [string()], boolean()) ->
  {args(), [string()]}.
maybe_add_value(Key, Next, Rest, true) ->
  {equal_sign_parse(Key), [Next | Rest]};
maybe_add_value(Key, Next, Rest, false) ->
  {{Key, Next}, Rest}.

-spec is_not_arg(string()) -> boolean().
is_not_arg(Data) ->
  not is_arg(Data).

-spec is_arg(string()) -> boolean().
is_arg([$-, $- | _]) ->
  true;
is_arg([$- | _]) ->
  true;
is_arg(_) ->
  false.

-spec equal_sign_parse(string()) -> args().
equal_sign_parse(Key) ->
  equal_sign_parse_evaluate(string:split(Key, "=", leading)).

-spec equal_sign_parse_evaluate(list(string())) -> args().
equal_sign_parse_evaluate([Key]) ->
  {Key, true};
equal_sign_parse_evaluate([NewKey, Data]) ->
  {NewKey, Data}.
