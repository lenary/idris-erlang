-module(idris_erlang_rts).

-export([floor/1, ceil/1]).
-export([str_index/2, str_null/1]).

-export([ptr_null/1, ptr_eq/2]).

-export([print_str/2, read_str/1, read_chr/1]).
-export([file_open/2, file_close/1, file_flush/1, file_eof/1, file_error/1, file_poll/1]).


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

%% Strings

% Just prevents some hacks in the code generator
-spec str_index(string(), integer()) -> integer().
str_index(Str, Idx) ->
    lists:nth(Idx+1, Str).

-spec str_null(string()) -> integer().
str_null([]) ->
    0;
str_null(_) ->
    1.

%% Pointers

-spec ptr_null(any()) -> integer().
ptr_null(undefined) ->
    0;
ptr_null(_) ->
    1.

-spec ptr_eq(any(), any()) -> integer().
ptr_eq(A,B) ->
    case A =:= B of
        true -> 0;
        false -> 1
    end.


%% IO Things. Mostly files, maybe some ports

-type handle() :: file:io_device() | undefined.

% Print a string exactly as it's provided, to a certain handle
-spec print_str(handle(), string()) -> integer().
print_str(undefined, _) ->
    1;
print_str(Handle, Str) ->
    case file:write(Handle, Str) of
        ok -> 0;
        _ -> 1
    end.

%% Read a line from the handle
-spec read_str(handle()) -> string().
read_str(undefined) ->
    "";
read_str(Handle) ->
    case file:read_line(Handle) of
        {ok, Data} -> Data;
        _ -> ""
    end.

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

-spec file_close(handle()) -> integer().
file_close(undefined) ->
    0;
file_close(Handle) ->
    case file:close(Handle) of 
        ok -> 0;
        _ -> 1
    end.

-spec file_flush(handle()) -> integer().
file_flush(undefined) ->
    0;
file_flush(Handle) -> 
    case file:sync(Handle) of
        ok -> 0;
        _ -> -1 
    end.

% This is really hacky. We have to do a read to find out if we're at
% the EOF, so we do a 1-char read, then scan back by one char. If the
% read or the scan fail, we say we're at the end, otherwise we use
% real info to see if we're at the eof.
-spec file_eof(handle()) -> integer().
file_eof(undefined) ->
    0; %% Null is at EOF
file_eof(Handle) ->
    case file:read(Handle,1) of
        eof -> 0; %% At EOF
        {ok, _} -> case file:position(Handle, {cur, -1}) of
                       {ok, _} -> -1; %% Not at EOF
                       {error, _} -> 0 %% Error Scanning Back -> EOF
                   end;
        {error, _} -> 0 %% Error -> EOF
    end.                         

%% In erlang, no files have errors... or something
-spec file_error(handle()) -> integer().
file_error(undefined) ->
    0;
file_error(_Handle) ->
    0.

%% And none are ready for reading, ever.
-spec file_poll(handle()) -> integer().
file_poll(_Handle) -> 0.

