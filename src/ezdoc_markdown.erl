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

-module(ezdoc_markdown).

-export([export/1]).

-spec export(ezdoc:ast()) -> iodata().
export(AST) ->
	export(AST, []).

export([], ["\n"|Acc]) ->
	lists:reverse(Acc);
export([Block|Tail], Acc) ->
	export(Tail, ["\n", block(Block)|Acc]).

block({h1, Text}) ->
	Text2 = inline(Text),
	[Text2, "\n", string:copies("=", iolist_size(Text2)), "\n"];
block({h2, Text}) ->
	Text2 = inline(Text),
	[Text2, "\n", string:copies("-", iolist_size(Text2)), "\n"];
block({h3, Text}) ->
	["### ", inline(Text), "\n"];
block({q, AST}) ->
	String = lists:droplast(unicode:characters_to_list(export(AST, []))),
	["> ", [begin case C of
		$\n -> ["\n> "];
		_ -> C
	end end || C <- String], "\n"];
block({cb, Language, Lines}) ->
	["``` ", Language, [["\n", L] || L <- Lines], "\n```\n"];
block({u, Items}) ->
	list(Items, "", []);
block({t, Head, Rows}) ->
	Head2 = [inline(H) || {c, H} <- Head],
	Rows2 = [[inline(T) || {c, T} <- Cells] || {r, Cells} <- Rows],
	Sizes = tuple_to_list(
		cols_size(Rows2, list_to_tuple([iolist_size(H) || H <- Head2]))),
	Head3 = cols_pad(Head2, Sizes, []),
	Rows3 = [cols_pad(Cells, Sizes, []) || Cells <- Rows2],
	["|", [[" ", H, " |"] || H <- Head3], "\n",
		"|", [[[" ", string:chars($-, iolist_size(H)), " |"] || H <- Head3]], "\n",
		[begin
			["|", [[" ", T, " |"] || T <- Cells], "\n"]
		end || Cells <- Rows3]];
block({p, Text}) ->
	[wrap(inline(Text), 72, ""), "\n"].

list([], _, Acc) ->
	lists:reverse(Acc);
list([{i, Text}|Tail], Indent, Acc) ->
	list(Tail, Indent, [[Indent, "*   ",
		wrap(inline(Text), 68 - length(Indent), Indent ++ "    "), "\n"]|Acc]);
list([{u, Items}|Tail], Indent, Acc) ->
	list(Tail, Indent, [list(Items, Indent ++ "    ", [])|Acc]).

inline(Text) when is_binary(Text) ->
	Text;
inline({ci, Text}) ->
	["`", Text, "`"];
inline({img, URL}) ->
	["![", URL, "](", URL, ")"];
inline({img, URL, Description}) ->
	["![", Description, "](", URL, ")"];
inline({l, URL}) ->
	["[", URL, "](", URL, ")"];
inline({l, URL, Description}) ->
	["[", Description, "](", URL, ")"];
inline(Text) ->
	[inline(T) || T <- Text].

cols_size([], Sizes) ->
	Sizes;
cols_size([Row|Tail], Sizes) ->
	Sizes2 = cols_size_row(Row, Sizes, 1),
	cols_size(Tail, Sizes2).

cols_size_row([], Sizes, _) ->
	Sizes;
cols_size_row([Cell|Tail], Sizes, N) ->
	CellSize = iolist_size(Cell),
	Sizes2 = if
		CellSize > element(N, Sizes) ->
			setelement(N, Sizes, CellSize);
		true ->
			Sizes
	end,
	cols_size_row(Tail, Sizes2, N + 1).

cols_pad([], [], Acc) ->
	lists:reverse(Acc);
cols_pad([H|HTail], [S|STail], Acc) ->
	cols_pad(HTail, STail, [[H, string:chars($\s, S - iolist_size(H))]|Acc]).

wrap(Text, Len, Prefix) ->
	String = unicode:characters_to_list(Text),
	[wrap_cut(String, Len, Prefix)].

wrap_with_prefix(String, Len, Prefix) ->
	[Prefix, wrap_cut(String, Len, Prefix)].

wrap_cut(String, Len, _) when length(String) =< Len ->
	String;
wrap_cut(String, Len, Prefix) ->
	Cut = string:rchr(string:substr(String, 1, Len), $\s),
	case Cut of
		0 ->
			String;
		_ ->
			[string:substr(String, 1, Cut), "\n",
				wrap_with_prefix(string:substr(String, Cut + 1), Len, Prefix)]
	end.
