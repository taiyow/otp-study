-module(bank5).
%-behaviour(gen_server).

% public API
-export([start/0, start_link/0, deposit/2, draw/2, get/1, failure/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% - failure: 破綻しているかフラグ
% - accounts: 個人ごとの残高
-record(state,
	{failure :: boolean(),
	 accounts = dict:new()}).

-define(BANKNAME, bank5). 

%% public API
%% - Ruby で言うところの module method

start() ->
    gen_server:start({local, ?BANKNAME},
		     ?MODULE, #state{failure=false}, []).

start_link() ->
    gen_server:start_link({local, ?BANKNAME},
			  ?MODULE, #state{failure=false}, []).

%% public API
%% - Ruby で言うところの instance method
deposit(Person, Money) when is_integer(Money) ->
    case gen_server:call(?BANKNAME, {deposit, Person, Money}) of
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    end.

draw(Person, Money) when is_integer(Money) ->
    case gen_server:call(?BANKNAME, {draw, Person, Money}) of
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    end.

get(Person)  ->
    case gen_server:call(?BANKNAME, {get, Person}) of
	{ok,Balance} -> show_balance(Balance);
	{error,Reason} -> io:format("error: ~p~n", [Reason])
    end.

failure() ->
    gen_server:cast(?BANKNAME, failure).

%% gen_server callbacks
%% - gen_server が外部のmoduleなのでここもpublicにする必要がある

init(State) ->
    {ok, State}.

handle_call({deposit, _, _}, _From, #state{failure=Failure}=State)
  when Failure ->
    {reply, {error, bank_is_failured}, State};
handle_call({deposit, Person, Money}, _From, State) ->
    Accounts = State#state.accounts,
    case update_balance(Person, Money, Accounts) of
	{error, no_such_account, _} ->
	    NewAccounts = dict:store(Person, Money, Accounts),
	    {reply, {ok, Money}, State#state{accounts=NewAccounts}};
	{Result, Arg, NewAccounts} ->
	    {reply, {Result, Arg}, State#state{accounts=NewAccounts}}
    end;

handle_call({draw, _, _}, _From, #state{failure=Failure}=State)
  when Failure ->
    {reply, {error, bank_is_failured}, State};

handle_call({draw, Person, Money}, _From, State) ->
    Accounts = State#state.accounts,
    {Result, Arg, NewAccounts} = update_balance(Person, -Money, Accounts),
    {reply, {Result, Arg}, State#state{accounts=NewAccounts}};

handle_call({get, _}, _From, #state{failure=Failure}=State)
  when Failure ->
    {reply, {ok, 0}, State};

handle_call({get, Person}, _From, State) -> 
    Accounts = State#state.accounts,
    case dict:find(Person, Accounts) of
	error ->
	    {reply, {error, no_such_account}, State};
	{ok, Balance} ->
	    {reply, {ok, Balance}, State}
    end;

handle_call(Msg, _From, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_cast(failure, State) ->
    {noreply, State#state{failure=true}};

handle_cast(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned.
    {ok, State}.

terminate(normal, _State) ->
    io:format("terminate~n"),
    ok.

%% private functions

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
