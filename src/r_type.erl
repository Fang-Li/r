-module(r_type).

%% API
-export([
    is_remote_process_alive/1,
    get_priv/1,
    getTimestamp/0,
    list_to_map/1,
    md5/1,
    rpc_call/2,
    unpad/1,
    rpc_cast/2,
    ip_to_list/1,
    to_binary/1,
    to_integer/1,
    to_atom/1,
    maps2dict/1,
    dict2maps/1,
    to_list/1,
    shuffle/1,
    lists_separate/2,
    list_find_list/2,
    fill_zero/2,
    trans_version/1,
    show_uid/1,
    gen_key/1,
    get_int_from_string/1,
    to_float/1,
    bin_join/2,
    bin_append/1,
    get_local_time/0,
    get_local_hour_time/0
]).

maps2dict(Maps) ->
    dict:from_list(maps:to_list(Maps)).

dict2maps(Dict) ->
    maps:from_list(dict:to_list(Dict)).


list_to_map(List) ->
    list_to_map(List, #{}).
list_to_map([], Rtn) ->
    Rtn;
list_to_map([_], Rtn) ->
    Rtn;
list_to_map([K, V | List], Box) ->
    list_to_map(List, Box#{to_atom(K) => V}).

get_priv(Module) ->
    [_ | Left1] = re:split(code:which(Module), "/", [{return, list}]),
    [_ | Left2] = lists:reverse(Left1),
    PrivDirPre = string:join(lists:reverse(Left2), "/"),
    "/" ++ PrivDirPre ++ "/../priv".

to_binary(undefined) ->
    undefined;
to_binary(Data) when is_list(Data) ->
    deref(unicode:characters_to_binary(Data));
to_binary(Data) when is_binary(Data) ->
    deref(Data);
to_binary(Data) when is_integer(Data) ->
    deref(erlang:integer_to_binary(Data));
to_binary(Data) when is_float(Data) ->
    deref(erlang:float_to_binary(Data, [{decimals, 2}]));
to_binary(Data) when is_atom(Data) ->
    deref(erlang:atom_to_binary(Data, latin1)).

to_integer(Data) when is_integer(Data) ->
    Data;
to_integer(Data) when is_list(Data) ->
    erlang:list_to_integer(Data);
to_integer(Data) when is_binary(Data) ->
    case catch erlang:binary_to_integer(Data) of
        Int when is_integer(Int) ->
            Int;
        _ ->
            erlang:binary_to_float(Data)
    end.

to_atom(Data) when is_atom(Data) ->
    Data;
to_atom(Data) when is_binary(Data) ->
    erlang:binary_to_atom(Data, latin1);
to_atom(Data) when is_list(Data) ->
    erlang:list_to_atom(Data).

to_list(Data) when is_list(Data) -> Data;
to_list(Data) when is_binary(Data) ->
    erlang:binary_to_list(Data);
to_list(Data) when is_atom(Data) ->
    erlang:atom_to_list(Data);
to_list(Data) when is_integer(Data) ->
    erlang:integer_to_list(Data);
to_list(Data) when is_float(Data) ->
    erlang:float_to_list(Data).

deref(Binary) ->
    case binary:referenced_byte_size(Binary) of
        Large when Large > 2 * byte_size(Binary) ->
            binary:copy(Binary);
        _ ->
            Binary
    end.

md5(S) ->
    Md5_bin = erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].


hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).

getTimestamp() ->
    {A, B, _} = os:timestamp(),
    A * 1000000 + B.

%%获取当前日期的年月日
get_local_time() ->
    {{Y, M, D}, {_, _, _}} = calendar:local_time(),
    erlang:list_to_binary(fill_zero(Y, 4) ++ fill_zero(M, 2) ++ fill_zero(D, 2)).
%%获取当前日期的年月日
get_local_hour_time() ->
    {{Y, M, D}, {H, _, _}} = calendar:local_time(),
    erlang:list_to_binary(fill_zero(Y, 4) ++ fill_zero(M, 2) ++ fill_zero(D, 2) ++ fill_zero(H, 2)).

-spec ip_to_list(inet:ip_address() | undefined |
{inet:ip_address(), inet:port_number()}) -> binary().

ip_to_list({IP, _Port}) ->
    ip_to_list(IP);
%% This function clause could use inet_parse too:
ip_to_list(undefined) ->
    <<"unknown">>;
ip_to_list(IP) ->
    list_to_binary(inet_parse:ntoa(IP)).

rpc_call(Node, FunArgs) ->
    rpc:call(Node, erlang, apply, FunArgs).
rpc_cast(Node, FunArgs) ->
    rpc:cast(Node, erlang, apply, FunArgs).

unpad(<<>>) ->
    <<>>;
unpad(Bin) ->
    Last = binary:last(Bin),
    Size = byte_size(Bin) - Last,
    RemSize = Size rem 16,

    case Bin of
        <<Data:Size/binary, 1>> when RemSize == 15 -> Data;
        <<Data:Size/binary, 2, 2>> when RemSize == 14 -> Data;
        <<Data:Size/binary, 3, 3, 3>> when RemSize == 13 -> Data;
        <<Data:Size/binary, 4, 4, 4, 4>> when RemSize == 12 -> Data;
        <<Data:Size/binary, 5, 5, 5, 5, 5>> when RemSize == 11 -> Data;
        <<Data:Size/binary, 6, 6, 6, 6, 6, 6>> when RemSize == 10 -> Data;
        <<Data:Size/binary, 7, 7, 7, 7, 7, 7, 7>> when RemSize == 9 -> Data;
        <<Data:Size/binary, 8, 8, 8, 8, 8, 8, 8, 8>> when RemSize == 8 -> Data;
        <<Data:Size/binary, 9, 9, 9, 9, 9, 9, 9, 9, 9>> when RemSize == 7 -> Data;
        <<Data:Size/binary, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10>> when RemSize == 6 -> Data;
        <<Data:Size/binary, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11>> when RemSize == 5 -> Data;
        <<Data:Size/binary, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12>> when RemSize == 4 -> Data;
        <<Data:Size/binary, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13>> when RemSize == 3 -> Data;
        <<Data:Size/binary, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14>> when RemSize == 2 -> Data;
        <<Data:Size/binary, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15>> when RemSize == 1 -> Data;
        <<Data:Size/binary, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16>> when RemSize == 0 -> Data;
        _ -> erlang:error(bad_padding)
    end.

is_remote_process_alive(PID) when is_pid(PID) ->
    Self = node(),
    case erlang:node(PID) of
        Self ->
            erlang:is_process_alive(PID);
        Node ->
            case catch rpc_call(Node, [erlang, is_process_alive, [PID]]) of
                Boolean when is_boolean(Boolean) ->
                    Boolean;
                _ ->
                    false
            end
    end;
is_remote_process_alive(_) ->
    false.

shuffle(L) ->
    shuffle(list_to_tuple(L), length(L)).

shuffle(T, 0) ->
    tuple_to_list(T);
shuffle(T, Len) ->
    Rand = random:uniform(Len),
    A = element(Len, T),
    B = element(Rand, T),
    T1 = setelement(Len, T, B),
    T2 = setelement(Rand, T1, A),
    shuffle(T2, Len - 1).

lists_separate(Fun, List) when is_function(Fun) andalso is_list(List) ->
    F =
        fun(X, {Trus, Falses}) ->
            case Fun(X) of
                true ->
                    [X | Trus];
                _ ->
                    [X | Falses]
            end
        end,
    lists:foldl(F, {[], []}, List);

lists_separate(List, Numer) ->
    LstLen = erlang:length(List),
    if
        Numer >= LstLen -> {List, []};
        true ->
            lists_separate(List, Numer, [], 0)
    end.
lists_separate(List, Numer, Rtn, Numer) ->
    {lists:reverse(Rtn), List};
lists_separate([H | T], Numer, Rtn, Count) ->
    lists_separate(T, Numer, [H | Rtn], Count + 1).

list_find_list([], _Key) ->
    false;
list_find_list([H | T], Key) when is_list(H) ->
    HD = erlang:hd(H),
    if
        Key == HD -> {ok, H};
        true -> list_find_list(T, Key)
    end;
list_find_list([_H | T], Key) ->
    list_find_list(T, Key).

fill_zero(N, Width) ->
    left_fill(N, Width, $0).

left_fill(N, Width, Fill) when is_integer(N) ->
    left_fill(integer_to_list(N), Width, Fill);
left_fill(N, Width, _Fill) when length(N) >= Width ->
    N;
left_fill(N, Width, Fill) ->
    left_fill([Fill | N], Width, Fill).

trans_version([V1, V2, V3 | _]) ->
    V1B = to_binary(V1),
    V2B = to_binary(V2),
    V3B = to_binary(V3),
    Head = <<"v">>,
    Sep = <<"_">>,
    <<Head/binary, V1B/binary, Sep/binary, V2B/binary, Sep/binary, V3B/binary>>.



-spec show_uid(Uid) ->
    binary() when
    Uid :: integer() | binary() | string().
show_uid(Uid) ->
    UidBin = to_binary(Uid),
    case UidBin of
        <<"R", NUid/binary>> ->
            NUid;
        _ ->
            UidBin
    end.

%%--------------------------------------------------------------------
%% @doc 生成redis key
%% @end
%%--------------------------------------------------------------------
-spec gen_key(List) ->
    binary() when
    List :: [binary()].
gen_key(List) ->
    gen_key(List, <<"">>).
gen_key([], Fin) ->
    Fin;
gen_key([Data], Fin) ->
    DataBin = to_binary(Data),
    <<Fin/binary, DataBin/binary>>;
gen_key([Data | List], Fin) ->
    DataBin = to_binary(Data),
    NewFin = <<Fin/binary, DataBin/binary, <<"#">>/binary>>,
    gen_key(List, NewFin).


get_int_from_string(InputString) ->
    String = unicode:characters_to_list(InputString),
    F =
        fun
            (S, L) ->
                SS = erlang:list_to_binary([S]),
                case re:run(SS, "^[0-9]+$", [unicode]) of
                    nomatch ->
                        L;
                    _ ->
                        [S | L]
                end
        end,
    erlang:list_to_binary(lists:foldr(F, [], String)).

to_float(Number) when is_number(Number) ->
    Number;
to_float(List) when is_list(List) ->
    to_float(to_binary(List));
to_float(Binary) when is_binary(Binary) ->
    case re:split(Binary, "[.]", [{return, binary}]) of
        [_] ->
            erlang:binary_to_integer(Binary);
        [_ | _] ->
            erlang:binary_to_float(Binary)
    end.

%%--------------------------------------------------------------------
%% @doc 通过一个binary分隔符把binary列表连接起来
%%      in : ([<<"a">>,<<"b">>], <<",">>) | out : <<"a,b">>
%% @end
%%--------------------------------------------------------------------
-spec bin_join(BinaryList, Separator) ->
    binary() when
    BinaryList :: [binary()], Separator :: binary().
bin_join([], Sep) when is_binary(Sep) ->
    <<"">>;
bin_join([H | T], Sep) when is_binary(Sep) ->
    iolist_to_binary([H, iolist_to_binary([iolist_to_binary([Sep, X]) || X <- T])]).


%%--------------------------------------------------------------------
%% @doc 连接binary
%%      in : ([<<"a">>,<<"b">>]) | out : <<"ab">>
%% @end
%%--------------------------------------------------------------------
-spec bin_append(BinaryList) ->
    binary() when
    BinaryList :: [binary()].
bin_append([]) ->
    <<"">>;
bin_append(Data) ->
    bin_append(to_list(Data), <<"">>).

bin_append([], Bin) ->
    Bin;
bin_append([HdData | TlData], Bin) ->
    HdDataBin = to_binary(HdData),
    bin_append(TlData, <<Bin/binary, HdDataBin/binary>>).
