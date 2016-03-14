プロセス管理とOTPのサンプル
===========================

簡単なサーバープロセスから、機能を追加しつつ、OTPに乗り換えることで機能と安全性が上がることを示したい。

以下の流れで機能拡張していく:

```
bank1 - 監視無し、API無し(直接送信)、戻り値無し
↓ (APIと戻り値対応)
bank2
↓ (監視を追加)
bank3
↓ (ループ変数をrecordに)
bank4
```


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

残高を人ごとに分けて銀行っぽく。
また、「破綻」APIを追加。
recordを使ってみる。

```
1> c(bank4).
{ok,bank4}
2> bank4:start().  % 開始
<0.45.0>
4> bank4:deposit(taiyo, 500).   % 500円を預ける
current balance is 500
ok
5> bank4:get(taiyo).            % 私の残高は500円です
current balance is 500
ok
6> bank4:deposit(akimaru, 10000).   % 秋丸さん(リッチ)は10000円を預ける
current balance is 10000
ok
7> bank4:get(taiyo).                % 私の残高は増えない
current balance is 500
ok
8> bank4:get(akimaru).              % 秋丸さんはリッチ
current balance is 10000
ok
9> bank4:draw(taiyo,1000).          % 自分の残高を超えては引き出せない
error: balance_is_short
ok
20> whereis(bank).
<0.46.0>
25> exit(whereis(bank), reason).    % bankを殺す
true
26> whereis(bank).                  % reborn
<0.74.0>
27> bank4:get(akimaru).             % でも残高は初期状態…
error: no_such_account
ok
```

failure (破綻)機能:
```
> c(bank4).
{ok,bank4}
> bank4:start().
<0.45.0>
> bank4:deposit(akimaru, 20000).   % お金を預けたあとで
current balance is 20000
ok
> bank4:failure().                 % 銀行が破綻すると
failure
> bank4:deposit(akimaru, 10000).   % 預金も
error: bank_is_failured
ok
> bank4:draw(akimaru, 10000).      % 引き出しもできない
error: bank_is_failured
ok
> bank4:get(akimaru).              % 残高は0円に
current balance is 0
ok
```

bank5.erl
---------

ここまでの不満点。
+ loop関数が長い
+ 同期/非同期APIの使い分けが面倒
+ send/receiveの細かい点に気を使いたくない
+ 同期APIのコードがDRY原則違反じゃない？

→ よかろう、ならばOTPだ。

…実際にOTP化してみての感想:
+ コードそんなに綺麗になってないよね、I/Fと実装の位置が遠くなっただけ？
+ handle_call の From 引数いらなくない？ → 実際 ejabberd でもデバッグログ用にしか使ってない
+ recordの部分が気持ち悪い → 同意、慣れるしかない
+ behaviour 宣言いらなくない？ → はい、callback関数の漏れチェックにしか使われてないよ
+ そもそもコードが手続型っぽくない？ → すまんな、もうちょっと、下位関数の値をそのまま流用すれば関数型っぽくなるが、これが限界。
+ bankプロセスを殺したら復活してこない → bank6 へ

bank5_sup.erl
---------

supervisor を導入。

supervisor と gen_server は両方とも init/1 を期待するので 1 つのモジュールで共存できないので、別モジュールを作成。

bank5_sup:start_link().
を実行するだけ。銀行は何度殺してもよみがえるさ！
