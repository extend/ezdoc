%% Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ezdoc).

-export([parse/1]).
-export([parse_file/1]).

-type code_inline() :: {ci, binary()}.
-type image() :: {img, binary()} | {img, binary(), binary()}.
-type link() :: {l, binary()} | {l, binary(), binary()}.
-type text() :: binary() | [binary() | code_inline() | image() | link()].

-type h1() :: {h1, text()}.
-type h2() :: {h2, text()}.
-type h3() :: {h3, text()}.
-type quote() :: {q, ast()}.
-type code_block() :: {cb, binary(), [binary()]}.
-type paragraph() :: {p, text()}.

-type list_item() :: {i, text()}.
-type unsorted_list() :: {u, [list_item() | unsorted_list()]}.

-type table_cell() :: {c, text()}.
-type table_row() :: {r, [table_cell()]}.
-type table() :: {t, [table_cell()], [table_row()]}.

-type block() :: h1() | h2() | h3()
	| quote() | code_block() | paragraph()
	| unsorted_list() | table().

-type ast() :: [block()].
-export_type([ast/0]).

-spec parse(binary()) -> ast().
parse(Data) when is_binary(Data) ->
	Lines = binary:split(Data, <<"\n">>, [global]),
	parse(Lines, []).

-spec parse_file(file:name_all()) -> ast().
parse_file(Filename) ->
	{ok, Data} = file:read_file(Filename),
	parse(Data).

parse([], Acc) ->
	lists:reverse(Acc);
parse([<<"::: ", T/bits>>|Tail], Acc) ->
	title(Tail, Acc, T, h1);
parse([<<":: ", T/bits>>|Tail], Acc) ->
	title(Tail, Acc, T, h2);
parse([<<": ", T/bits>>|Tail], Acc) ->
	title(Tail, Acc, T, h3);
parse(Lines = [<<"\t", _/bits>>|_], Acc) ->
	quote(Lines, Acc, []);
parse(Lines = [<<"* ", _/bits>>|_], Acc) ->
	{Tail, BLines} = bullets_split(Lines, []),
	parse(Tail, [bullets(BLines, [])|Acc]);
parse([<<"``` ", Lang/bits>>|Tail], Acc) ->
	code_block(Tail, Acc, Lang, []);
parse(Lines = [<<"||\t", _/bits>>|_], Acc) ->
	table(Lines, Acc);
parse([<<>>|Tail], Acc) ->
	parse(Tail, Acc);
parse(Lines, Acc) ->
	paragraph(Lines, Acc, []).

title(Lines, Acc, H, Type) ->
	{Tail, H2} = extended(Lines, [H]),
	parse(Tail, [{Type, inline(H2)}|Acc]).

paragraph([<<>>|Tail], Acc, PAcc) ->
	parse(Tail, [{p, inline(paragraph_merge(lists:reverse(PAcc), <<>>))}|Acc]);
paragraph([P|Tail], Acc, PAcc) ->
	paragraph(Tail, Acc, [P|PAcc]).

paragraph_merge([], << $\s, Acc/bits >>) ->
	Acc;
paragraph_merge([L|Tail], Acc) ->
	paragraph_merge(Tail, << Acc/binary, $\s, L/binary >>).

quote([<<>>|Tail], Acc, QuAcc) ->
	quote(Tail, Acc, [<<>>|QuAcc]);
quote([<<"\t", L/bits>>|Tail], Acc, QuAcc) ->
	quote(Tail, Acc, [L|QuAcc]);
quote(Lines, Acc, QuAcc) ->
	Quote = parse(lists:reverse([<<>>|QuAcc]), []),
	parse(Lines, [{q, Quote}|Acc]).

bullets_split([<<"*", L/bits>>|Tail], Acc) ->
	{Tail2, L2} = extended(Tail, [L]),
	bullets_split(Tail2, [L2|Acc]);
bullets_split(Tail, Acc) ->
	{Tail, lists:reverse(Acc)}.

bullets([], BAcc) ->
	{u, lists:reverse(BAcc)};
bullets(Lines = [<<"* ", _/bits>>|_], BAcc) ->
	{Tail, BLines} = bullets_split(Lines, []),
	bullets(Tail, [bullets(BLines, [])|BAcc]);
bullets([<< " ", Item/bits >>|Tail], BAcc) ->
	bullets(Tail, [{i, inline(Item)}|BAcc]).

code_block([<<"```">>|Tail], Acc, Lang, CoAcc) ->
	parse(Tail, [{cb, Lang, lists:reverse(CoAcc)}|Acc]);
code_block([C|Tail], Acc, Lang, CoAcc) ->
	code_block(Tail, Acc, Lang, [C|CoAcc]).

table([<< "||\t", Rest/bits >>, <<"|">>|Lines], Acc) ->
	Head = table_next(Rest, []),
	{Body, Tail} = table_rows(Lines, []),
	parse(Tail, [{t, Head, Body}|Acc]).

table_rows([<<>>|Tail], Acc) ->
	{lists:reverse(Acc), Tail};
table_rows([<< "|\t", Rest/bits >>|Tail], Acc) ->
	table_rows(Tail, [{r, table_next(Rest, [])}|Acc]).

table_next(<<>>, Acc) when length(Acc) > 0 ->
	lists:reverse(Acc);
table_next(<< $\t, Rest/bits >>, Acc) ->
	table_next(Rest, Acc);
table_next(Rest, Acc) ->
	table_col(Rest, Acc, <<>>).

table_col(<<>>, Acc, Col) ->
	lists:reverse([{c, inline(Col)}|Acc]);
table_col(<< $\t, Rest/bits >>, Acc, Col) ->
	table_next(Rest, [{c, inline(Col)}|Acc]);
table_col(<< C, Rest/bits >>, Acc, Col) ->
	table_col(Rest, Acc, << Col/binary, C >>).

extended([<< "\t", T/bits >>|Tail], Acc) ->
	extended(Tail, [[<<$\s>>, T]|Acc]);
extended(Lines, Acc) ->
	{Lines, iolist_to_binary(lists:reverse(Acc))}.

inline(Text) ->
	Acc = inline(Text, [], <<>>),
	Acc2 = [T || T <- Acc, T =/= <<>>],
	case Acc2 of
		[Bin] when is_binary(Bin) -> Bin;
		_ -> Acc2
	end.

inline(<<>>, Acc, <<>>) ->
	lists:reverse(Acc);
inline(<<>>, Acc, BinAcc) ->
	lists:reverse([BinAcc|Acc]);
inline(<< $`, Rest/bits >>, Acc, BinAcc) ->
	code_inline(Rest, [BinAcc|Acc], <<>>);
inline(<< $^, $!, Rest/bits >>, Acc, BinAcc) ->
	link1(Rest, [BinAcc|Acc], undefined, img, <<>>);
inline(<< $^, $", Rest/bits >>, Acc, BinAcc) ->
	link2(Rest, [BinAcc|Acc], <<>>);
inline(<< $^, Rest/bits >>, Acc, BinAcc) ->
	link1(Rest, [BinAcc|Acc], undefined, l, <<>>);
inline(<< C, Rest/bits >>, Acc, BinAcc) ->
	inline(Rest, Acc, << BinAcc/binary, C >>).

code_inline(<< $`, Rest/bits >>, Acc, BinAcc) ->
	inline(Rest, [{ci, BinAcc}|Acc], <<>>);
code_inline(<< C, Rest/bits >>, Acc, BinAcc) ->
	code_inline(Rest, Acc, << BinAcc/binary, C >>).

link1(<<>>, Acc, undefined, Tag, BinAcc) ->
	lists:reverse([{Tag, BinAcc}|Acc]);
link1(<<>>, Acc, Text, Tag, BinAcc) ->
	lists:reverse([{Tag, BinAcc, Text}|Acc]);
link1(Rest = << $\s, _/bits >>, Acc, undefined, Tag, BinAcc) ->
	inline(Rest, [{Tag, BinAcc}|Acc], <<>>);
link1(Rest = << $\s, _/bits >>, Acc, Text, Tag, BinAcc) ->
	inline(Rest, [{Tag, BinAcc, Text}|Acc], <<>>);
link1(<< $^, Rest/bits >>, Acc, undefined, Tag, BinAcc) ->
	inline(Rest, [{Tag, BinAcc}|Acc], <<>>);
link1(<< $^, Rest/bits >>, Acc, Text, Tag, BinAcc) ->
	inline(Rest, [{Tag, BinAcc, Text}|Acc], <<>>);
link1(<< C, Rest/bits >>, Acc, Text, Tag, BinAcc) ->
	link1(Rest, Acc, Text, Tag, << BinAcc/binary, C >>).

link2(<< $^, $!, Rest/bits >>, Acc, BinAcc) ->
	link1(Rest, Acc, BinAcc, img, <<>>);
link2(<< $^, Rest/bits >>, Acc, BinAcc) ->
	link1(Rest, Acc, BinAcc, l, <<>>);
link2(<< C, Rest/bits >>, Acc, BinAcc) ->
	link2(Rest, Acc, << BinAcc/binary, C >>).
