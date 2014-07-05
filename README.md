EZDoc
=====

EZDoc is a simplistic documentation format and associated parser.

EZDoc is meant to be a very simple syntax for writing documentation. It 
lets the developer focus on actually writing rather than dealing with 
syntax elements. The syntax is very small and easy to remember, 
including the syntax for tables.

EZDoc's parser returns an AST that can then be manipulated to generate 
outputs in many different formats. EZDoc makes no assumption about the 
semantics of the document, instead, you can decide when processing the 
AST that one element means one thing or another.

Because it is so easy to convert to another format, it makes perfect 
sense to start writing using EZDoc and then convert to a more complex 
format when more formatting options are needed.

Getting started
---------------

EZDoc defines the following block elements:

*   Title
*   Quote
*   Code block
*   Paragraph
*   Unsorted list
*   Table

EZDoc also defines the following inline elements:

*   Code inline
*   Link
*   Image

Empty lines are used as block separators and are not included in the 
resulting AST, with the exception of code blocks. Also note that quotes 
can contain empty lines which do not end the quote, although the empty 
lines are later discarded from the AST.

An EZDoc file is expected to end with an empty line. EZDoc files use 
the `.ezdoc` extension.

Block elements
--------------

Block elements are elements found at the top level of the document. 
They are expected to be separated by at least one empty line.

Most block elements can include inline elements, though not all.

### Title

You can create a title by starting a line with one, two or three colon 
characters. The number of colon characters indicates the importance of 
the title, with three being the most important.

A title can span multiple lines by making the subsequent lines start 
with one or more tab characters.

``` ezdoc
::: This could be the title of the document

:: This could be the title of a section of the document

: This could be used to clearly separate parts of a section

: This title is very long so we use a tab to indicate that
	it spans more than one line.
```

The AST is `{h1, Title}` for the most important title, `{h2, Title}` 
for the second title and `{h3, Title}` for the less important one. The 
`Title` text can contain inline elements.

### Quote

You can create a quote by starting a line with a tab. Quotes are 
special in that they can contain any other element. They basically can 
be seen as a new level in the document.

An empty line does not end a quote. A quote ends when any other block 
element is found.

``` ezdoc
This is a paragraph in the first level of the document.

	This creates a new level. This is a quote block.

	This is still part of the same quote block.

	* This is a list inside the quote block.
	* Another element in the list.

Back to the first level.
```

The AST for this element is `{q, AST}` where `AST` is a list of any 
block element.

### Code block

You can embed code in a document by creating a code block. A code block 
starts with a line starting with three backticks followed by a space 
which is itself followed by the language the code is written in. The 
language value is mandatory.

A code block is closed by a line that only contains three backticks.

The following example shows an Erlang code block.

``` ezdoc
``` erlang
identity(X) ->
	X.
``` 
```

A code block can be represented inside another code block, the trick is 
to simply add a space after the closing backticks so that it is not 
considered the end of the original block.

The AST for this element is `{cb, Lang, Lines}` where `Lang` is a 
binary for the language and `Lines` is a list of binaries, one for each 
line inside the code block. Empty lines are preserved.

### Unsorted list

An unsorted list can be created by starting a line with an asterisk 
character. A list item is one element of the list. Every line that 
starts with an asterisk character is a list item.

A list can contain another list, allowing you to have lists of lists. 
There is no limit to how deep a list can get. To create a deeper list, 
simply start the next line with an additional asterisk character.

List elements can span multiple lines by making the subsequent lines 
start with one or more tab characters.

``` ezdoc
* This is the first list element.
* This is the second element.
** This is the first element of the new list.
** Another element. This one is very long so like most block
	elements, we can use a tab to indicate it spans more than
	one line.
** And one last element.
```

The AST for unsorted lists is `{u, Elements}` where `Elements` is a 
list of unsorted lists and list elements. The AST for list elements is 
`{i, Text}` where `Text` is the text for this list element. It can 
contain any inline element.

### Table

A table can be created by starting a line with two pipe characters 
followed by a tab.

The first line of a table is the table head. The second line of a table 
must only include a single pipe character with nothing else on the 
line. This is used to clearly distinguish the table head from its body. 
Subsequent lines are rows. Row lines start with a single pipe character 
followed by a tab.

All lines in a table are basically a list of cells that contain text. 
Cells are separated by one or more tab. It is not necessary to properly 
align cells in all lines, although it looks better when done so. A cell 
cannot be empty, it must contain at least one non-tab character.

``` ezdoc
||	First col	Second col		Third col
|
|	123			one two three	Some text with `inline` stuff.
|	456			four five six	More text.
```

The AST for the table is `{t, Head, Body}` where `Head` is a list of 
cells and `Body` is a list of rows in the body. The AST for table rows 
is `{r, Cells}` where `Cells` is a list of cells in the row. Finally, 
the AST for cells is `{c, Text}` where `Text` is the content of the 
cell, which may contain inline elements.

### Paragraph

A paragraph is created when no other block could be identified. It can 
start with text or an inline element. A paragraph ends with an empty 
line.

``` ezdoc
This is a paragraph. Yes it is as simple as that. A paragraph
can take as many lines as you want.

This is another paragraph.
```

The AST for this element is `{p, Text}` where `Text` is the content of 
the paragraph. The content can contain any inline element.

Inline elements
---------------

Inline elements are syntax elements found directly inside text.

Text AST can take two forms: either it is simply a binary, which means 
it doesn't contain inline elements; or it is a list of binaries, code 
inline or link elements.

### Emphasis

You can put *emphasis* on text by wrapping it with asterisk characters. 
There is only one kind of emphasis.

``` ezdoc
Listen carefully because I am telling you something *very*
important. You *better* remember.
```

The AST for this element is `{e, EnclosedText}` where `EnclosedText` is 
a binary.

### Code inline

You can wrap text around backticks to create a code inline element. It 
is generally used to identify source code text inside otherwise normal 
text.

``` ezdoc
This is an example paragraph in the `EZDoc` format. The use
of backticks here creates a `code inline` element.
```

The AST for this element is `{ci, EnclosedText}` where `EnclosedText` 
is a binary.

### Link

You can create links in two different ways depending on whether the 
link has a description or not.

Links without a description start with a caret character and are 
followed by the URL until a caret or whitespace is encountered.

Links with a description start with a caret character immediately 
followed by a double quote character and then by the description. The 
description ends when the caret character is encountered. It then 
follows the same rule as links without a description.

``` ezdoc
This is a ^"link with a description^http://example.org/lwad and
one without follows ^http://example.org/owf^. The termination
is optional, we could also write  ^http://example.org/owf with
no problem.

You can also create short links like ^anotherpage and it will
work just fine.
```

The AST for this element is `{l, URL}` for the short form and `{l, URL, 
Description}` for the long form, with `URL` and `Description` both 
binaries.

### Image

You can embed images in a way that is very similar to creating links. 
Embedded images are meant to be displayed directly inside the document, 
although all formats may not support them.

Like links, there are two ways to create images: with or without a 
description. Images also use the caret character, the only difference 
is that an exclamation mark is placed between the caret and the URL.

``` ezdoc
This is an ^"embedded image with a description^!image.png and
one without follows ^!image.png^. The termination is once again
optional, we could write ^!image.png with no problem.
```

The AST for this element is `{img, URL}` for the short form and `{img, 
URL, Description}` for the long form, with `URL` and `Description` both 
binaries.

Examples
--------

You can find the source for this document in `doc/src/README.ezd`.

Support
-------

Official IRC channel: #ninenines on irc.freenode.net
