-module(bank4).
-export([start/0, restarter/0, loop/1, deposit/2, draw/2, get/1, failure/0]).

% 預金者を区別できるように。
% 銀行破たん機能追加
-record(state,
	{failure :: boolean(),
	 accounts = dict:new()}).

%% public API
start() ->
    % 再起動担当プロセスを作成
    spawn(?MODULE, restarter, []).

restarter() ->
    % link先の死亡の巻き添えをふせぐ
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, loop, [#state{failure=false}]),
    register(bank, Pid),
    receive
	{'EXIT', Pid, normal} -> % normal exit
	    ok;
	{'EXIT', Pid, shutdown} -> % exit by API
	    ok;
	{'EXIT', _Pid, _} -> % unwilling exit, then restart again
	    restarter()
    end.	     

deposit(Person, Money) when is_integer(Money) ->
    bank ! {self(), deposit, Person, Money},
    receive
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    after 500 ->
	    io:format("timeout!!~n")
    end.

draw(Person, Money) when is_integer(Money) ->
    bank ! {self(), draw, Person, Money},
    receive
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    after 500 ->
	    io:format("timeout!!~n")
    end.

get(Person)  ->
    bank ! {self(), get, Person},
    receive
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    after 500 ->
	    io:format("timeout!!~n")
    end.

failure()  ->
    bank ! failure.   % 非同期API

%% private loop

loop(#state{failure=Failure,accounts=Accounts}=State) ->
    receive
	{From, deposit, _, _} when Failure ->
	    From ! {error, bank_is_failured},
	    loop(State);
	{From, deposit, Person, Money} when is_integer(Money) ->
	    case update_balance(Person, Money, Accounts) of
		{error, no_such_account, _} ->
		    NewAccounts = dict:store(Person, Money, Accounts),
		    From ! {ok, Money};
		{error, Reason, NewAccounts} ->
		    From ! {error, Reason};
		{ok, NewBalance, NewAccounts} ->
		    From ! {ok, NewBalance}
	    end,
	    loop(State#state{accounts=NewAccounts});
	{From, draw, _, _} when Failure ->
	    From ! {error, bank_is_failured},
	    loop(State);
	{From, draw, Person, Money} when is_integer(Money) ->
	    case update_balance(Person, -Money, Accounts) of
		{error, Reason, NewAccounts} ->
		    From ! {error, Reason};
		{ok, NewBalance, NewAccounts} ->
		    From ! {ok, NewBalance}
	    end,
	    loop(State#state{accounts=NewAccounts});
	{From, get, _} when Failure ->
	    From ! {ok, 0},
	    loop(State);
	{From, get, Person} ->
	    case dict:find(Person, Accounts) of
		error ->
		    From ! {error, no_such_account};
		{ok, Balance} ->
		    From ! {ok, Balance}
	    end;
	failure ->
	    loop(State#state{failure=true});
	_ ->
	    io:format("invalid message~n"),
	    loop(State)
    end.
    
show_balance(Balance) when is_integer(Balance) ->
    io:format("current balance is ~p~n", [Balance]);
show_balance(X) ->
    io:format("invalid arugment: ~p~n", [X]).

update_balance(Person, Money, Accounts) ->
    case dict:find(Person, Accounts) of
	error ->
	    {error, no_such_account, Accounts};
	{ok, Balance} -> 
	    NewBalance = Balance + Money,
	    if
		NewBalance < 0 ->
		    {error, balance_is_short, Accounts};
		NewBalance >= 0 ->
		    NewAccounts = dict:store(Person, NewBalance, Accounts),
		    {ok, NewBalance, NewAccounts}
	    end
    end.
