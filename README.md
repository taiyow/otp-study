プロセス管理とOTPのサンプル
===========================

bank1.erl
---------

* 監視無し
* API無し → 使う側が直接メッセージを送信
* 非同期処理のみ → エラー情報が返ってこない

```
% 開始処理
> c(bank1).
{ok,bank1}
> Pid1 = bank1:start().
balance is 0
<0.211.0>  % <- bank1:start/0 内で spawn/3 したプロセスのpid

% 預金する
> Pid1 ! {deposit,500}.
balance is 500
{deposit,500}  % <- これは送信したメッセージをshellが表示している

% 引き出す
> Pid1 ! {draw,250}.
balance is 250
{draw,250}

% 残高がマイナスにはなれない
> Pid1 ! {draw,300}.
balance is less than money!  %<- loop側で表示
{draw,300}
balance is 250

% きっかり引き出す
> Pid1 ! {draw,250}.
balance is 0
{draw,250}
```

bank2.erl
---------

* 監視無し
* API有り
* 同期処理を(のみを)サポート
* 戻り値として現在の残高を返す (残高表示はAPI呼び出し側で行う)

```
> c(bank2).
{ok,bank2}
> Pid2 = bank2:start().
<0.45.0>
> bank2:get(Pid2).
current balance is 0  % <- APIを呼んだ側(shell process)で表示
ok
> bank2:deposit(Pid2, 500).
current balance is 500
ok
> bank2:draw(Pid2, 250).
current balance is 250
ok
> bank2:draw(Pid2, 300).
error: balance_is_short  % <- API が error tupple を返した
ok
> bank2:get(Pid2).
current balance is 250
ok

> exit(Pid2, abort).    % bank loop を殺してみる
true
> process_info(Pid2).   % processがもう存在しない
undefined
9> bank2:get(Pid2).     % API呼び出しはtimeoutになる
timeout!!
ok
```

bank3.erl
---------

* 監視を追加 (link, register)

```
1> c(bank3).
{ok,bank3}
2> bank3:start().   % register するので pid は覚えなく良い
<0.45.0>
3> bank3:get().
current balance is 0
ok
4> bank3:deposit(500).
current balance is 500
ok
5> bank3:draw(250).
current balance is 250
ok
6> bank3:draw(300).
error: balance_is_short
ok
7> bank3:get().
current balance is 250
ok

8> bank3:start().   % registerは同時に重複できないのでエラーになる
<0.53.0>
9> 
=ERROR REPORT==== 6-Mar-2016::23:14:26 ===
Error in process <0.53.0> with exit value:
{badarg,[{erlang,register,[bank,<0.54.0>],[]},
         {bank3,restarter,0,[{file,"bank3.erl"},{line,19}]}]}

> whereis(bank).    % bankプロセスのpidはこちら
<0.46.0>
10> exit(whereis(bank), abort).   %bank を殺してみる
true
11> whereis(bank).  % あれ、bankが生まれ変わっている？
<0.57.0>
12> bank3:get().    % ただし過去の残高は覚えていない…
current balance is 0
ok
```

bank4.erl
---------

