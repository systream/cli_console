%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-type argument_type() :: flag | atom | string | binary | integer.

-record(argument, {name :: string(),
                   type :: argument_type(),
                   optional = true :: boolean(),
                   default :: term(),
                   description :: string() | undefined}).

-type(command_argument() :: #argument{}).

-type output_format() :: {text, string()} | {format, module(), term()}.

-type command_fun() :: fun((proplists:proplist()) -> [output_format()] | output_format() ).

-type command() :: string() | command_argument().

-export_type([command_argument/0, command/0]).
