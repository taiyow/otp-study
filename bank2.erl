-module(bank2).
-export([start/0, loop/1, deposit/2, draw/2, get/1]).

% ちょっとましなループ
% - API作成
% - 非同期ループの変数が一つだけなので、預金者が区別できない

%% public API
start() ->
    spawn(?MODULE, loop, [0]).

show_balance(Balance) when is_integer(Balance) ->
    io:format("current balance is ~p~n", [Balance]);
show_balance(X) ->
    io:format("invalid arugment: ~p~n", [X]).


deposit(Pid, Money) when is_integer(Money) ->
    Pid ! {self(), deposit, Money},
    receive
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    after 500 ->
	    io:format("timeout!!~n")
    end.

draw(Pid, Money) when is_integer(Money) ->
    Pid ! {self(), draw, Money},
    receive
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    after 500 ->
	    io:format("timeout!!~n")
    end.

get(Pid)  ->
    Pid ! {self(), get},
    receive
	{ok,Balance} -> show_balance(Balance)
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


    
    
