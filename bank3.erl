-module(bank3).
-export([start/0, restarter/0, loop/1, deposit/1, draw/1, get/0]).

% 独自supervisor(再起動のみ)を導入
% - link, process_flag 設定
% - サーバーが孫プロセスになるので、register する
% - 非同期ループの変数が一つだけなので、預金者が区別できない

%% public API
start() ->
    % 再起動担当プロセスを作成
    spawn(?MODULE, restarter, []).


restarter() ->
    % link先の死亡の巻き添えをふせぐ
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, loop, [0]),
    register(bank, Pid),
    receive
	{'EXIT', Pid, normal} -> % normal exit
	    ok;
	{'EXIT', Pid, shutdown} -> % exit by API
	    ok;
	{'EXIT', _Pid, _} -> % unwilling exit, then restart again
	    restarter()
    end.	     

deposit(Money) when is_integer(Money) ->
    bank ! {self(), deposit, Money},
    receive
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    after 500 ->
	    io:format("timeout!!~n")
    end.

draw(Money) when is_integer(Money) ->
    bank ! {self(), draw, Money},
    receive
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    after 500 ->
	    io:format("timeout!!~n")
    end.

get()  ->
    bank ! {self(), get},
    receive
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    after 500 ->
	    io:format("timeout!!~n")
    end.


%% private loop

loop(Balance) ->
    receive 
	{From, deposit, Money} when is_integer(Money) ->
	    NewBalance = Balance + Money,
	    From ! {ok, NewBalance},
	    loop(NewBalance);
	{From, draw, Money} when is_integer(Money) andalso Balance > Money ->
	    NewBalance = Balance - Money,
	    From ! {ok, NewBalance},
	    loop(NewBalance);
	{From, draw, Money} when is_integer(Money) ->
	    From ! {error, balance_is_short},
	    loop(Balance);
	{From, get} ->
	    From ! {ok, Balance},
	    loop(Balance);
	_ ->
	    io:format("invalid message~n"),
	    loop(Balance)
    end.
    
show_balance(Balance) when is_integer(Balance) ->
    io:format("current balance is ~p~n", [Balance]);
show_balance(X) ->
    io:format("invalid arugment: ~p~n", [X]).


