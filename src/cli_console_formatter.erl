%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_console_formatter).
-author("Peter Tihanyi").

-define(LINE_LENGTH_LIMIT, 63).

-type colors() :: red | green | yellow | white.

-behavior(cli_console_output).

%% API
-export([text/1, text/2,
         title/1,
         separator/0,
         format/1,
         alert/1,
         list/1,
         table/1,
         success/1,
         error/1,
         warning/1]).

-spec text(string()) -> {format, module(), {text, string()}}.
text(String) ->
  {format, ?MODULE, {text, String}}.

-spec text(string(), list(term())) -> {format, module(), {text, string()}}.
text(String, Args) ->
  {format, ?MODULE, {text, io_lib:format(String, Args)}}.

-spec title(string()) -> {format, module(), {title, string()}}.
title(String) ->
  {format, ?MODULE, {title, String}}.

-spec alert(string()) -> {format, module(), {alert, string()}}.
alert(String) ->
  {format, ?MODULE, {alert, String}}.

-spec success(string()) -> {format, module(), {success, string()}}.
success(String) ->
  {format, ?MODULE, {success, String}}.

-spec error(string()) -> {format, module(), {error, string()}}.
error(String) ->
  {format, ?MODULE, {error, String}}.

-spec warning(string()) -> {format, module(), {warning, string()}}.
warning(String) ->
  {format, ?MODULE, {warning, String}}.

-spec list([string()]) -> {format, module(), {list, [string()]}}.
list(List) when is_list(List) ->
  {format, ?MODULE, {list, List}}.

-spec table(Table) -> {format, module(), {table, Table}} when
  Table :: [#{string() => string()}].
table(Data) when is_list(Data) ->
  {format, ?MODULE, {table, Data}}.

-spec separator() -> {format, module(), {text, string()}}.
separator() ->
  text([$- || _ <- lists:seq(1, ?LINE_LENGTH_LIMIT)]).

-spec format(Format) -> string() when
  Format :: {text, string()} | {title, string()} | {alert, string()} |
            {success, string()} | {error | string()} | {list, [string()]} |
            {table, [#{string() => string()}]}.
format({text, Data}) ->
  Data ++ "~n";
format({title, Data}) ->
  color_bold(white, string:titlecase(Data) ++ "~n");
format({alert, Data}) ->
  lists:flatten(color_bold(red, Data ++ "~n"));
format({warning, Data}) ->
  lists:flatten(color_bold(yellow, Data ++ "~n"));
format({success, Data}) ->
  lists:flatten(color_bold(green, Data ++ "~n"));
format({error, Data}) ->
  lists:flatten(color_bold(red, Data ++ "~n"));
format({list, Data}) ->
  lists:flatten(lists:map(fun(Line) -> " * " ++ list_line(Line) ++ "~n" end, Data));
format({table, Data}) ->
  TableHeaders = get_table_header(Data),
  TableWidth = get_table_width(TableHeaders),
  lists:flatten(table_horizontal_separator(TableWidth) ++
                render_header(TableHeaders) ++
                table_horizontal_separator(TableWidth) ++
                render_rows(TableHeaders, Data) ++
                table_horizontal_separator(TableWidth)
  ).

-spec list_line(string()) -> string().
list_line(Line) when length(Line) > ?LINE_LENGTH_LIMIT ->
  % @todo: word wrap here
  string:sub_string(Line, 1, ?LINE_LENGTH_LIMIT) ++ "~n   " ++
  list_line(string:sub_string(Line, ?LINE_LENGTH_LIMIT));
list_line(Line) ->
  Line.

get_table_header(Data) ->
  get_table_header(Data, #{}).

get_table_header([], Acc) ->
  Keys = maps:keys(Acc),
  maps:to_list(merge_if_wider([{Key, length(Key)} || Key <- Keys], Acc));
get_table_header([Row | RestOfRows], Acc) when is_map(Row) ->
  get_table_header([maps:to_list(Row) | RestOfRows], Acc);
get_table_header([Row | RestOfRows], Acc) ->
  CellsWidth = get_cells_width(Row),
  get_table_header(RestOfRows, merge_if_wider(CellsWidth, Acc)).

merge_if_wider([], Acc) ->
  Acc;
merge_if_wider([{Key, CellWidth} | Rest], Acc) ->
  case maps:get(Key, Acc, 0) of
    ActualCellWidth when ActualCellWidth < CellWidth ->
      merge_if_wider(Rest, Acc#{Key => CellWidth});
    _ ->
      merge_if_wider(Rest, Acc)
  end.

get_cells_width(Row) ->
  Keys = proplists:get_keys(Row),
  [{Key, length(proplists:get_value(Key, Row))} || Key <- Keys].

render_rows(TableHeaders, Data) ->
  lists:map(fun(CData) -> render_row(TableHeaders, CData) end, Data).

render_row(HeaderData, Data) ->
  render_row(HeaderData, Data, []).

render_header(Data) ->
  render_header(Data, []).

render_header([], Acc) ->
  Acc ++ " |~n";
render_header([{Key, Width} | Rest], Acc) ->
  Line  = " | " ++ color(white, string:pad(Key, Width+3, both)),
  render_header(Rest, Acc ++ Line).

render_row([], _Data, Acc) ->
  Acc ++ " |~n";
render_row(Header, Data, Acc) when is_map(Data) ->
  render_row(Header, maps:to_list(Data), Acc);
render_row([{Key, Width} | Rest], Data, Acc) ->
  Line  = " | " ++ string:pad(proplists:get_value(Key, Data, ""), Width+3, both),
  render_row(Rest, Data, Acc ++ Line).

-spec get_table_width(proplists:proplist()) -> pos_integer().
get_table_width(TableHeaders) ->
  lists:foldl(fun({_, Width}, Total) -> Width+6+Total end, 1, TableHeaders).

-spec table_horizontal_separator(pos_integer()) -> string().
table_horizontal_separator(Width) ->
  " " ++ [$- || _ <- lists:seq(1, Width)] ++ " ~n".

-spec color(colors(), string()) -> string().
color(Color, Text) ->
  "\e[" ++ color_to_code(Color) ++ "m" ++ lists:flatten(Text) ++ "\e[0m".

-spec color_bold(colors(), string()) -> string().
color_bold(Color, Text) ->
  "\e[" ++ color_to_code(Color) ++ ";1m" ++ lists:flatten(Text) ++ "\e[0m".

-spec color_to_code(colors()) -> string().
color_to_code(red) ->
  "31";
color_to_code(green) ->
  "32";
color_to_code(yellow) ->
  "33";
color_to_code(white) ->
  "37".
