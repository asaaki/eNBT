-module(nbt).

-export([
  open_plain_file/1,
  open_gzip_file/1,

  read_file/1,
  read_plain_file/1,
  read_gzip_file/1,

  unconsult/2,
  consult/1,

  parse_data/1
]).


%% OPEN AND READ FILE

open_plain_file(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  Bin.

open_gzip_file(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  zlib:gunzip(Bin).


read_plain_file(Filename) ->
  Res = parse_data(open_plain_file(Filename)),
  {nbt, Res}.

read_gzip_file(Filename) ->
  Res = parse_data(open_gzip_file(Filename)),
  {nbt, Res}.

read_file(Filename) ->
  read_gzip_file(Filename).


%% HELPER

% writing erlang io list in readable format to file
% can be reimported by nbt:consult/1
unconsult(File, NBTdata) ->
  {ok, S} = file:open(File, write),
  {nbt, Data} = NBTdata,
  lists:foreach(fun(X) -> io:format(S, "~p.~n" ,[X]) end, Data),
  file:close(S).

% for convenience
consult(Filename) ->
  {ok, Data} = file:consult(Filename),
  {nbt, Data}.

%% PARSE DATA

parse_data(Data) ->
  [<<>>, ParsedData] = parse_data(Data, 0, 0),
  ParsedData.

parse_data(Data, LvlP, LvlC) ->
  parse_data(Data, [], LvlP, LvlC).


parse_data(<<>>, Parsed, L, L) ->
  % end of file and block -> ok!
  [<<>>, Parsed];
parse_data(<<>>, Parsed, L, L) ->
  % end of file (lvl different -> should throw err)
  [<<>>, Parsed];
parse_data(Data, [], L, L) ->
  %% enter new data block
  NewLvl = L + 1,
  parse_data(Data, newblock, L, NewLvl)
  ;
parse_data(Data, Parsed, L, L) ->
  %% leave data block
  [Data, Parsed]
  ;
parse_data(Data, Parsed, LvlP, LvlC) ->
  P = case Parsed of
    newblock -> [];
    Any -> Any
  end,
  {Type, Data0} = parse_nbt_type(Data),
  case Type of
    0 ->
      %% TAG_END
      parse_data(Data0, P, LvlC, LvlC);
    9 ->
      %% TAG_LIST
      {Name, Payload} = parse_nbt_name(Data0),
      [List, Rest] = parse_nbt_list(Payload, Name, LvlC),
      NewParsed = P++[List],
      parse_data(Rest, NewParsed, LvlP, LvlC);
    10 ->
      %% TAG_COMPOUND
      {Name, Payload} = parse_nbt_name(Data0),
      [Compound, Rest] = parse_nbt_compound(Payload, Name, LvlC),
      NewParsed = P++[Compound],
      parse_data(Rest, NewParsed, LvlP, LvlC);
    T ->
      %% EVERYTHING ELSE
      {Name, Payload} = parse_nbt_name(Data0),
      [{Ctype, Cdata}, Rest] = parse_nbt_content(T, Payload, LvlC),
      Node = {Ctype, Name, Cdata},
      NewParsed = P++[Node],
      parse_data(Rest, NewParsed, LvlP, LvlC)
  end.

parse_nbt_type(<<>>) ->
  {0, <<>>};
parse_nbt_type(<<Type:8/unsigned-integer, RestLoad/binary>>) ->
  <<Type:8/unsigned-integer, RestLoad/binary>> = Payload,
  {Type, RestLoad}.

parse_nbt_name(<<>>) ->
  [eof,<<>>];
parse_nbt_name(<<Nlen:16/unsigned-integer, Tmp/binary>>) ->
  Size = Nlen * 8,
  <<Name:Size/binary-unit:1, RestLoad/binary>> = Tmp,
  {Name, RestLoad}.

parse_nbt_content(_, <<>>, _) ->
  [<<>>,<<>>];
parse_nbt_content(Type, Payload, LvlC) ->
  %% 0 (TAG_END) is not handled here
  case Type of
    1  -> parse_nbt_byte(Payload);
    2  -> parse_nbt_short(Payload);
    3  -> parse_nbt_int(Payload);
    4  -> parse_nbt_long(Payload);
    5  -> parse_nbt_float(Payload);
    6  -> parse_nbt_double(Payload);
    7  -> parse_nbt_byte_array(Payload);
    8  -> parse_nbt_string(Payload);
    9  -> parse_nbt_list(Payload, none, LvlC);
    10 -> parse_nbt_compound(Payload, none, LvlC);
    11 -> parse_nbt_int_array(Payload);
    _  -> {error, undef_type}
  end.


%% TYPE PARSER nesting

parse_nbt_list(<<ListType:8/unsigned-integer, Icount:32/signed-integer, Tmp/binary>>, Name, Lvl) ->
  [Rest, Parsed] = parse_nbt_nameless(ListType, Tmp, Lvl, Icount),
  [{list, Name, Parsed}, Rest].

parse_nbt_nameless(Type, Payload, Lvl, Icount) ->
  parse_nbt_nameless(Type, Payload, Lvl, Icount, []).

parse_nbt_nameless(_Type, RestLoad, _, 0, Acc) ->
  [RestLoad, lists:reverse(Acc)];
parse_nbt_nameless(Type, Payload, Lvl, Icount, Acc) ->
  [Value, Rest] = parse_nbt_content(Type, Payload, Lvl),
  NewIcount = Icount - 1,
  parse_nbt_nameless(Type, Rest, Lvl, NewIcount, [Value|Acc]).

parse_nbt_compound(Payload, Name, Lvl) ->
  [Rest, Parsed] = parse_data(Payload, [], Lvl, Lvl),
  [{compound, Name, Parsed}, Rest].


%% TYPE PARSER normal content

parse_nbt_byte(<<Result:8/binary-unit:1, RestLoad/binary>>) ->
  [{byte, Result}, RestLoad].

parse_nbt_short(<<Result:16/signed-integer, RestLoad/binary>>) ->
  [{short, Result}, RestLoad].

parse_nbt_int(<<Result:32/signed-integer, RestLoad/binary>>) ->
  [{int, Result}, RestLoad].

parse_nbt_long(<<Result:64/signed-integer, RestLoad/binary>>) ->
  [{long, Result}, RestLoad].

parse_nbt_float(<<Result:32/float, RestLoad/binary>>) ->
  [{float, Result}, RestLoad].

parse_nbt_double(<<Result:64/float, RestLoad/binary>>) ->
  [{double, Result}, RestLoad].

parse_nbt_byte_array(<<Prefix:32/signed-integer, Tmp/binary>>) ->
  Size = Prefix * 8,
  <<Bytes:Size/binary-unit:1, RestLoad/binary>> = Tmp,
  Result = binary:bin_to_list(Bytes),
  [{byte_array, Result}, RestLoad].

parse_nbt_string(<<Prefix:16/unsigned-integer, Tmp/binary>>) ->
  Size = Prefix * 8,
  <<String:Size/binary-unit:1, RestLoad/binary>> = Tmp,
  Result = unicode:characters_to_list(String),
  [{string, Result}, RestLoad].

parse_nbt_int_array(<<Prefix:32/signed-integer, Tmp/binary>>) ->
  Size = Prefix * 8,
  <<Ints:Size/binary-unit:4, RestLoad/binary>> = Tmp,
  Result = bytes_to_ints(Ints),
  [{int_array, Result}, RestLoad].

bytes_to_ints(Bytes) ->
  case parse_nbt_int(Bytes) of
    {Int, <<>>} -> [Int];
    {Int, Rest} -> [Int|bytes_to_ints(Rest)]
  end.
