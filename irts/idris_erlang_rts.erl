-module(idris_erlang_rts).

-define(TRUE, 0).
-define(FALSE, 1).

-export([floor/1, ceil/1]).
-export([bool_cast/1]).
-export([str_index/2, str_null/1]).
-export([ptr_null/1, ptr_eq/2]).

-export([write_str/1, write_file/2, read_str/0, read_file/1, read_chr/1]).
-export([file_open/2, file_close/1, file_flush/1, file_eof/1,
	 file_error/1, file_poll/1]).

-type idr_bool() :: ?TRUE | ?FALSE.

%%% This is a set of helper wrappers for the Idris Runtime System Most
%%% are used for primitives, but there's some other helpers in here
%%% too.

%% Erlang Doesn't have Floor and Ceil, so we have our own
%% implementations from
%% http://erlangcentral.org/wiki/index.php/Floating_Point_Rounding

-spec floor(number()) -> integer().
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

-spec ceil(number()) -> integer().
ceil(X) when X < 0 ->
    trunc(X);
ceil(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

-spec bool_cast(boolean()) -> idr_bool().
bool_cast(true) -> ?TRUE;
bool_cast(_)    -> ?FALSE.

%% Strings

% Just prevents some hacks in the code generator
-spec str_index(string(), integer()) -> integer().
str_index(Str, Idx) ->
    lists:nth(Idx+1, Str).

-spec str_null(string()) -> idr_bool().
str_null([]) ->
    ?TRUE;
str_null(_) ->
    ?FALSE.

%% Pointers

-spec ptr_null(any()) -> idr_bool().
ptr_null(undefined) ->
    ?TRUE;
ptr_null(_) ->
    ?FALSE.

-spec ptr_eq(any(), any()) -> idr_bool().
ptr_eq(A,B) ->
    bool_cast(A =:= B).


%% IO Things. Mostly files, maybe some ports

-type handle() :: file:io_device() | undefined.

% Print a string exactly as it's provided, to a certain handle
-spec write_file(handle(), string()) -> idr_bool().
write_file(undefined, _) ->
    ?FALSE;
write_file(Handle, Str) ->
    case file:write(Handle, Str) of
        ok -> ?TRUE;
        _ -> ?FALSE
    end.

-spec write_str(string()) -> idr_bool().
write_str(Str) ->
    write_file(standard_io, Str).

-spec read_file(handle()) -> string().
read_file(undefined) ->
    "";
read_file(Handle) ->
    case file:read_line(Handle) of
        {ok, Data} -> Data;
        _ -> ""
    end.

%% Read a line from the handle
-spec read_str() -> string().
read_str() ->
    read_file(standard_io).

-spec read_chr(handle()) -> integer().
read_chr(undefined) ->
    -1;
read_chr(Handle) ->
    case file:read(Handle, 1) of
        {ok, [Chr]} -> Chr;
        _ -> -1
    end.

-spec file_open(string(), string()) -> handle().
file_open(Name, Mode) ->
    ModeOpts = case Mode of
                   "r" ->  [read];
                   "w" ->  [write];
                   "r+" -> [read, write]
               end,
    case file:open(Name, ModeOpts) of
        {ok, Handle} -> Handle;
        _ -> undefined
    end.

-spec file_close(handle()) -> idr_bool().
file_close(undefined) ->
    ?FALSE;
file_close(Handle) ->
    case file:close(Handle) of
        ok -> ?TRUE;
        _ -> ?FALSE
    end.

-spec file_flush(handle()) -> idr_bool().
file_flush(undefined) ->
    ?FALSE;
file_flush(Handle) ->
    case file:sync(Handle) of
        ok -> ?TRUE;
        _ -> ?FALSE
    end.

% This is really hacky. We have to do a read to find out if we're at
% the EOF, so we do a 1-char read, then scan back by one char. If the
% read or the scan fail, we say we're at the end, otherwise we use
% real info to see if we're at the eof.
-spec file_eof(handle()) -> idr_bool().
file_eof(undefined) ->
    ?TRUE; %% Null is at EOF
file_eof(Handle) ->
    case file:read(Handle,1) of
        eof -> ?TRUE; %% At EOF
        {ok, _} -> case file:position(Handle, {cur, -1}) of
                       {ok, _} -> ?FALSE; %% Not at EOF
                       {error, _} -> ?TRUE %% Error Scanning Back -> EOF
                   end;
        {error, _} -> ?TRUE %% Error -> EOF
    end.

%% In erlang, no files have errors... or something
-spec file_error(handle()) -> idr_bool().
file_error(undefined) ->
    ?FALSE;
file_error(_Handle) ->
    ?FALSE.

%% And none are ready for reading, ever.
-spec file_poll(handle()) -> idr_bool().
file_poll(_Handle) -> ?FALSE.
