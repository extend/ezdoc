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

-module(ezdoc_man).

-export([export/2]).

-spec export(1..8, ezdoc:ast()) -> iodata().
export(Section, [{h1, Text}|AST]) ->
	Text2 = inline(Text),
	[".TH \"", Text2, "\" ", integer_to_list(Section), "\n.ta T 4n\n\\&\n.SH NAME\n\\&\n.LP\n", Text2,
		"\n.SH DESCRIPTION\n\\&\n", export2(AST, [])].

export2([], ["\n"|Acc]) ->
	lists:reverse(Acc);
export2([Block|Tail], Acc) ->
	export2(Tail, ["\n", block(Block)|Acc]).

block({h2, Text}) ->
	[".SH ", string:to_upper(unicode:characters_to_list(inline(Text))), "\n\\&"];
block({h3, Text}) ->
	[".SS ", unicode:characters_to_list(inline(Text)), "\n\\&"];
block({q, AST}) ->
	[".RS\n", export2(AST, []), "\n.RE\n"];
block({cb, _Language, Lines}) ->
	[".LP\n.nf\n", [["\n\t", L] || L <- Lines], "\n.fi\n"];
block({u, Items}) ->
	list(Items);
block({t, Head, Rows}) ->
	Head2 = [H || {c, H} <- Head],
	Rows2 = [[T || {c, T} <- Cells] || {r, Cells} <- Rows],
	Sizes = tuple_to_list(
		cols_size(Rows2, list_to_tuple([text_length(H) || H <- Head2]))),
	Head3 = cols_pad(Head2, Sizes, []),
	Rows3 = [cols_pad(Cells, Sizes, []) || Cells <- Rows2],
	[".RS 4\n.B\n", Head3, "\n.br\n",
		[begin
			[Cells, "\n.br\n"]
		end || Cells <- Rows3], ".RE\n"];
block({p, Text}) ->
	[".LP\n", inline(Text), "\n"].

list(Items) ->
	[".RS 4\n", [
		case Item of
			{i, I} ->
				[inline(I), "\n.br\n"];
			{u, U} ->
				list(U)
		end
	|| Item <- Items], "\n.RE\n"].

cols_size([], Sizes) ->
	Sizes;
cols_size([Row|Tail], Sizes) ->
	Sizes2 = cols_size_row(Row, Sizes, 1),
	cols_size(Tail, Sizes2).

cols_size_row([], Sizes, _) ->
	Sizes;
cols_size_row([Cell|Tail], Sizes, N) ->
	CellSize = text_length(Cell),
	Sizes2 = if
		CellSize > element(N, Sizes) ->
			setelement(N, Sizes, CellSize);
		true ->
			Sizes
	end,
	cols_size_row(Tail, Sizes2, N + 1).

cols_pad([H], [S], Acc) ->
	lists:reverse([[inline(H), string:chars($\s, S - text_length(H))]|Acc]);
cols_pad([H|HTail], [S|STail], Acc) ->
	cols_pad(HTail, STail, [[inline(H), string:chars($\s, S - text_length(H) + 2)]|Acc]).

inline(Text) when is_binary(Text) ->
	Text;
inline({e, Text}) ->
	["\\fB", Text, "\\fR\\&"];
inline({ci, Text}) ->
	["\\fI", Text, "\\fR\\&"];
inline({img, URL}) ->
	["\\fI", URL, "\\fR\\&"];
inline({img, URL, Description}) ->
	["\\fI", Description, " (", URL, ")\\fR\\&"];
inline({l, URL}) ->
	["\\fI", URL, "\\fR\\&"];
inline({l, URL, Description}) ->
	["\\fI", Description, " (", URL, ")\\fR\\&"];
inline(Text) ->
	[inline(T) || T <- Text].

text_length(Text) when is_binary(Text) ->
	length(unicode:characters_to_list(Text));
text_length({ci, Text}) ->
	text_length(Text);
text_length({img, URL}) ->
	text_length(URL);
text_length({img, URL, Description}) ->
	text_length(Description) + text_length(URL) + 3;
text_length({l, URL}) ->
	text_length(URL);
text_length({l, URL, Description}) ->
	text_length(Description) + text_length(URL) + 3;
text_length(Text) ->
	lists:sum([text_length(T) || T <- Text]).
