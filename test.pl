# -*- mode: prolog; -*-
:- encoding(utf8). % swi-prolog だとこんな感じに書いとくと使えるらしい、、、けどっターミナルだと使えん orz
/*
  test.pl
  テストプログラム
*/

% 日本語表示 addhock 述語定義
echo(X) :- spawn(echo, [X]).

% not の定義(プリミティブも在るよ ^ ^ )
not(X) :- X, !, false.
not(X).

% lesson 1-1-1
% Hello World 文字列(というか、、、アトムである f^^;)はシングルクォート
helloWorld :- print('Hello World').
% ダブルクォート は 文字リストに変換される こっちを strings type として扱って欲しいにゃぁ
helloWorld :- print("Hello World").
/*
gnu-prolog 日本語使えない orz... SWI-prolog は文字コード指定が出来るらしいんで、入れて そっち使うことにする
この 演習 ? はどっちでも動くように考慮する事とする
ダメだったぁ ので gnu-prolog でやりまーす ^ ^
*/
helloWorld :- print('はろーわーるど').
helloWorld :- print("はろーわーるど").
helloWorld :- echo('はろーわーるど').

% lesson 1-1-2 , 1-1-4
plus5_3 :- X is 5 + 3, print(X).

% lesson 1-1-3
printGramin :- Gramin = 'gramin', print(Gramin).
printGramin :- Gramin = 'グラミン', echo(Gramin).

% lesson 1-1-5
enzan :- A is 5, B is 3,
	 Hikizan is A + B,
	 Kakezan is A * B,
	 Warizan is A / B,
	 Amari is A mod B,
	 print(Hikizan), nl,
	 print(Kakezan), nl,
	 print(Warizan), nl,
	 print(Amari).

% lesson 1-1-6
menseki :- PI is 3.14159265,
		   X1 is 3 ^ 2 * PI, print(X1), nl,
		   X2 is 4 ^ 2 * PI, print(X2), nl,
		   X3 is 5 ^ 2 * PI, print(X3), nl.

% lesson 1-2-2
for10 :- for(10).
% for(0).
for(Count) :- Count =< 0, !, false.
for(Count) :- print('Hello'), nl, Count2 is Count - 1, for(Count2).
% その2 !
kurikaesi10 :- kurikasi(10).
% kurikaesi(0).
kurikaesi(Count) :- Count =< 0, !, false.
kurikaesi(Count) :- print('Hello'), nl, Count2 is Count - 1, kurikaesi(Count2).

% lesson 1-2-3
aisatu :- Greetings = ['Hello','こんにちは','你好','Guten tag'],
%	  nth(2, Greetings, Aisatu), % gnu-prolog original , not iso-prolog
	  item(2, Greetings, Aisatu),
	  echo(Aisatu).
% nth 自主実装 item
item(0, [_|_], _).
item(1, [Fast|R], Fast).
item(I, [Fast|R], X) :- I1 is I - 1, item(I1, R, X).

% lesson 1-2-5

% よく使うんで 述語定義 しとく
greetings(['Hello','こんにちは','你好','Guten tag']).

allGreetings :- greetings(L), repeatOneGreeting(L).
repeatOneGreeting([]).
repeatOneGreeting([Greeting|R]) :- echo(Greeting),
				   repeatOneGreeting(R).

% lesson 1-2-6
randomGreeting :- greetings(Greetings), random(1, 4, Random),
		  item(Random, Greetings, Greeting),
		  echo(Greeting).

% lesson 1-3-1〜2

/*
適当な行入力 組込述語 が見当たらないので作る
linput_to_atoms/1
タイピング入力: "hello world\n"
出力: [hello,world] <- こんな感じになる予定

prolog の武器 差分リスト を導入して使う dL([hoge|R],R) て感じとする
*/

linput_to_atoms(Atoms) :- read_chars(Chars),
			  chars_atoms(Chars, Atoms).
% sub
read_chars(Chars) :- get_key(Key),
		     rChars(Chars, dL([Key|R],R)).
% read_chars/1 sub
isLf(Code) :- char_code('\r', Code).
isNotLf(Code) :- isLf(LfCode), !, Code =\= LfCode.

rChars([], dL([Key|R],R)) :- isLf(Key).
rChars([Char|R], dL([Key|R1],R1)) :-
    isNotLf(Key), char_code(Char, Key),
    get_key(Key2),
    rChars(R, dL([Key2|R2],R2)).
% read_chars/1 end

chars_atoms([Char|CharsR], [Atom|AtomsR]) :-
    cs_css(dL([Char|CharsR],CharsR),
	   dL([dL([Chars|CharsR2],CharsR2)|CharsR3],CharsR2))
    /* cs{文字差分リスト}<->css{文字差分リストの差分リスト} 特定の文字で切分ける */ ,
    css_as.
% chars_atoms/2 sub
% cs_css
isIgnoreChar(Char) :- member(Char, [' ',',','.']).
% 1文字だけの場合
cs_css(dL([Char],[]), dL(dL([],[]),[])) :- isIgnoreChar(Char). % 無視チェック
cs_css(dL([Char],[]), dL(dL([Char],[]),[])). % 1文字 o.k. cs->css 変換
% 2文字以上の場合
% cs部の整理
cs_css(dL([Char1,Char2|CharsR],[Char2|CharsR]),
       dL(dL([Char2|CharsR],CharsR),CharsR3)) :-
    isIgnoreChar(Char1), % 1文字目無視チェック
    cs_css(dL([Char2|CharsR],CharsR),
	   dL([dl([Chars|CharsR2],CharsR2)|CharsR3],CharsR3)).
cs_css(dL([Char1,Char2|CharsR],[Char2|CharsR]),
       dL([dL([Chars|CharsR2],CharsR2)|CharsR3],CharsR3)) :-
    isIgnoreChar(Char2), % 2文字目無視チェック
    cs_css(dL([Char1|CharsR],CharsR),
	   dL([dl([Chars|CharsR2],CharsR2)|CharsR3],CharsR3)).
% cs部の整理 end
% css部作成
cs_css(dL([Char1,Char2|CharsR],[Char2|CharsR]),
       dL([dL([Chars|CharsR2],CharsR2)|CharsR3],CharsR3)) :-
    cs_css(dL([Char1,Char2|CharsR],[Char2|CharsR]), % cs部
	   dL(dL([Char1,Char2|CharsR],[Char2|CharsR]),CharsR3)). % css部
% css部作成 end
% cs_css end: dL(dL([Chars|[]),...) で出力
css_as .

% end sub


% end linput_to_atoms/1

% quick sort 実装してみる プリミティブも在るけどねっ
% qsort(バラバラ, 整列) n1<nX 数値のリストを対象とする
qsort(UnSortedList, SortedList) :- % qsort(in, out)
    get_pv(UnSortedList, PivotValue),
    qs(PivotValue, dL(UnSortedList, UnSortedList)).
qsort(UnSortedList, SortedList) :- % qsort(out, in)
    get_pv(SortedList, PivotValue),
    qs(PivotValue, dL(SortedList, SortedList)).
% sub
get_pv(L, PV) :- sum_list(L, Sum), number(L, N), PV is Sum/N. % 平均値
% 1個の時
qs(_, dL([I], [])).
% 2個以上の時
qs(PV, dL([I,I1|R], [I1|R])) :- I =< PV, qs(PV, dL([I,I1|R], R)).
qs(PV, dL([I,I1|R], [I1|R])) :- qs(PV, dL([I,I1|R], R))

.

qs(dL([N1,N2],[]), dL([N2,N1],[])).
% qs(dL(,), dL(,))
