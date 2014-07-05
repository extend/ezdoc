# See LICENSE for licensing information.

PROJECT = ezdoc
include erlang.mk

docs: all
	mkdir -p doc/man7 doc/html
	erl -noinput -pa ebin -eval "AST = ezdoc:parse_file(\"doc/src/README.ezd\"), file:write_file(\"doc/man7/ezdoc.1\", ezdoc_man:export(7, AST)), file:write_file(\"doc/html/ezdoc.html\", ezdoc_html:export(AST)), file:write_file(\"README.md\", ezdoc_markdown:export(AST)), init:stop()."
	gzip -f doc/man7/ezdoc.1
