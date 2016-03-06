-module(bank1).
-export([start/0, loop/1]).

% 原始的なループ
% - API無し、メッセージは使う側が直接送信
% - ループの変数が一つだけなので、預金者が区別できない
% - 非同期処理のみ

start() ->
    spawn(?MODULE, loop, [0]).

loop(Balance) ->
    io:format("balance is ~p~n", [Balance]),
    receive 
	{deposit, Money} when is_integer(Money) ->
	    loop(Balance+Money);
	{draw, Money} when is_integer(Money) andalso Balance >= Money ->
	    loop(Balance-Money);
	{draw, Money} when is_integer(Money) ->
	    io:format("balance is less than money!~n"),
	    loop(Balance);
	_ ->
	    io:format("invalid message~n"),
	    loop(Balance)
    end.

    
    
