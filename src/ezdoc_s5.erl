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

-module(ezdoc_s5).

-export([export/1]).

%% We expect a particular structure.
%%
%% The first few blocks fill in the first slide and the footer.
%%
%% They must follow this order:
%%   h1, p, h2, 4 elements list, h2, 2 elements list
%%
%% They correspond to:
%%   title, subtitle,
%%   ignored, author, contact info, company, catch phrase,
%%   ignored, conference name, talk date
%%
%% Past that point it gets simpler. Every h2 and h3 create a new slide.
%% An h2 slide is expected to be empty and starts a new section of the talk.
%% An h3 slide is expected to have content and that content is rendered
%% in the slide directly without any further constraints.
%%
%% Quote blocks are ignored and can be used to put comments in the source.

-spec export(ezdoc:ast()) -> iodata().
export([{h1, Title}, {p, Subtitle},
		{h2, _}, {u, [{i, Author}, {i, Contact}, {i, Company}, {i, CatchPhrase}]},
		{h2, _}, {u, [{i, Conference}, {i, Date}]}|AST]) ->
	[
		header(Title, Subtitle, Author, Contact, Company, CatchPhrase, Conference, Date),
		slides(AST, []),
		footer()
	].

header(Title, Subtitle, Author, Contact, Company, CatchPhrase, Conference, Date) ->
	["
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" 
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>D3 + Websocket for live Web applications</title>
<!-- metadata -->
<meta charset=\"utf8\" />
<meta name=\"generator\" content=\"S5\" />
<meta name=\"version\" content=\"S5 1.1\" />
<meta name=\"presdate\" content=\"", Date, "\" />
<meta name=\"author\" content=\"", Author, "\" />
<meta name=\"company\" content=\"", Company, "\" />
<!-- configuration parameters -->
<meta name=\"defaultView\" content=\"slideshow\" />
<meta name=\"controlVis\" content=\"visible\" />
<!-- style sheet links -->
<link rel=\"stylesheet\" href=\"ui/default/slides.css\" type=\"text/css\" media=\"projection\" id=\"slideProj\" />
<link rel=\"stylesheet\" href=\"ui/default/outline.css\" type=\"text/css\" media=\"screen\" id=\"outlineStyle\" />
<link rel=\"stylesheet\" href=\"ui/default/print.css\" type=\"text/css\" media=\"print\" id=\"slidePrint\" />
<link rel=\"stylesheet\" href=\"ui/default/opera.css\" type=\"text/css\" media=\"projection\" id=\"operaFix\" />
<link href=\"ui/sh/sh99s.css\" rel=\"stylesheet\"/>
<!-- S5 JS -->
<script src=\"ui/default/slides.js\" type=\"text/javascript\"></script>
<!-- syntax highlighter JS -->
<script type=\"text/javascript\" src=\"ui/sh/shCore.js\"></script>
<script type=\"text/javascript\" src=\"ui/sh/shBrushErlang.js\"></script>
<script type=\"text/javascript\" src=\"ui/sh/shBrushJScript.js\"></script>
<script type=\"text/javascript\" src=\"ui/sh/shBrushXml.js\"></script>
</head>
<body>

<div class=\"layout\">
<div id=\"controls\"><!-- DO NOT EDIT --></div>
<div id=\"currentSlide\"><!-- DO NOT EDIT --></div>
<div id=\"header\">
    <div id=\"sub_header\"></div>
    <div id=\"logo\"><img src=\"ui/img/logo.svg\"/></div>
</div>
<div id=\"footer\">
<div id=\"footer_shadow\"></div>
<h1>", Conference, "</h1>
<h2>", Title, ", ", Company, "</h2>
</div>

</div>


<div class=\"presentation\">

<div class=\"slide\">
<h1>", Title, "</h1>
<h2>", Subtitle, "</h2>
<h3>", Author, " - ", Contact, "</h3>
<h4>", CatchPhrase, "</h4>
</div>\n\n\n"].

footer() ->
	"</div>\n\n"
	"<script type=\"text/javascript\">SyntaxHighlighter.all();</script>\n\n"
	"</body>\n"
	"</html>\n".

slides([], Acc) ->
	lists:reverse(Acc);
slides([{h2, Title}|AST], Acc) ->
	slides(AST, [[
		"<div class=\"slide\">\n"
		"<h1>", inline(Title), "</h1>\n"
		"</div>\n\n\n"]|Acc]);
slides([{h3, Title}|AST], Acc) ->
	{AST2, Contents} = slide(AST, []),
	slides(AST2, [[
		"<div class=\"slide\">\n"
		"<h1>", inline(Title), "</h1>\n",
		Contents,
		"</div>\n\n\n"]|Acc]).

slide(AST = [], Acc) ->
	{AST, lists:reverse(Acc)};
slide(AST = [{h2, _}|_], Acc) ->
	{AST, lists:reverse(Acc)};
slide(AST = [{h3, _}|_], Acc) ->
	{AST, lists:reverse(Acc)};

slide([{cb, Language, Lines}|AST], Acc) ->
	slide(AST, [[
		"<div><script type=\"syntaxhighlighter\" class=\"brush: ",
		Language, "\"><![CDATA[",
		[["\n", L] || L <- Lines],
		"\n]]></script></div>"]|Acc]);
slide([{u, Items}|AST], Acc) ->
	slide(AST, [["<ul>\n", list(Items), "</ul>\n"]|Acc]);
slide([{p, Text}|AST], Acc) ->
	slide(AST, [["<p>", inline(Text), "</p>\n"]|Acc]);
slide([{q, _}|AST], Acc) ->
	slide(AST, Acc).

list([]) ->
	[];
list([{i, Item}, {u, List}|Tail]) ->
	["<li>", inline(Item), "<ul>\n", list(List), "</ul>\n</li>\n", list(Tail)];
list([{i, Item}|Tail]) ->
	["<li>", inline(Item), "</li>\n", list(Tail)].

inline(Text) when is_binary(Text) ->
	Text;
inline({ci, Text}) ->
	["<code>", Text, "</code>"];
inline({img, URL}) ->
	["<img src=\"pics/", URL, "\"/>"];
inline({img, URL, Description}) ->
	["<img title=\"", Description, "\" src=\"pics/", URL, "\"/>"];
inline({l, URL}) ->
	["<a href=\"", URL, "\">", URL, "</a>"];
inline({l, URL, Description}) ->
	["<a href=\"", URL, "\">", Description, "</a>"];
inline(Text) ->
	[inline(T) || T <- Text].
