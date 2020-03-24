-module(user_default).
-compile(export_all).

get_meta() ->
    user_default:module_info().
get_timestamp() ->
    {M, S, _} = erlang:timestamp(),
    M * 1000000 + S.
get() ->
    a.


%% 打印彩虹桥的输入输出
r() ->
    r_shell:recon(r_shell:mfa(), []).

r(Arg) ->
    r_shell:recon(mfa(Arg), []).

r(Arg1, Arg2) ->
    r_shell:recon(mfa(Arg1, Arg2), []).

r(Arg1, Arg2, Arg3) ->
    r_shell:recon(mfa(Arg1, Arg2, Arg3), []).


%% 跟踪指定pid
rpid(Pid) ->
    r_shell:recon(r_shell:mfa(), [{pid, Pid}]).

rpid(Pid, Arg) ->
    r_shell:recon(mfa(Arg), [{pid, Pid}]).

rpid(Pid, Arg1, Arg2) ->
    r_shell:recon(mfa(Arg1, Arg2), [{pid, Pid}]).

rpid(Pid, Arg1, Arg2, Arg3) ->
    r_shell:recon(mfa(Arg1, Arg2, Arg3), [{pid, Pid}]).

%% 跟踪指定一个模块,所有函数
-spec(mfa(atom()|list() | [{M :: atom()|list(), F :: atom()|list()}, ...]) -> list()).

mfa(Mod) when is_atom(Mod) ->
    [{Mod, '_'}];

%% 跟踪指定多个模块,指定函数
%% 跟踪多条匹配:一个模块,一个函数,一个pattern的任意组合形式
mfa(MFAs) when is_list(MFAs) ->
    MFAs.

%% 跟踪指定模块,特定函数
-spec(mfa(atom(), atom()|list()) -> list()).
mfa(Mod, Func) ->
    [{Mod, Func}].

%% 跟踪指定参数个数
-spec(mfa(atom()|list(), atom()|list(), integer()) -> list()).
mfa(Mod, Func, Args) ->
    [{Mod, Func, Args}].

%% 跟踪玩法开发中的相关协议,前后端通信协议数据 ro意指recon ocean相关
ro() ->
    r_shell:trace_without_ping().


%% 跟踪指定server的返回数据和所有的请求数据
-spec(rs(atom()|list()) -> integer()).
rs(Server) ->
    r_shell:trace_specified_server(Server).

%% 跟踪模块中导出的函数
re(Mods) when is_atom(Mods) orelse is_list(Mods) ->
    r_shell:recon([{Mods, '_'}], [{scope, export}]).

%% 停止跟踪,停止输出
cl() ->
    recon_trace:clear().

%% 调试打印
p(X) ->
    io:format("~p~n", [X]).

%% 调试打印Unicode中文
pu(X) ->
    io:format("~ts~n", [X]).
