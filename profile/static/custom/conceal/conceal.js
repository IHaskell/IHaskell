// Implement Haskell-Conceal for IPython notebook with IHaskell. 

"using strict";
var concealExtension = (function() {
    var Pos = CodeMirror.Pos;

    // Concealable elements
    var conceals = {
        "\\": "λ",
        ".": "∘",
        "/=": "≠",
        "::": "∷",
        ">>": "»",
        "<<": "«",
        "->": "→",
        "<-": "←",
        "<>": "•",
        "!!": "‼",
        "=>": "⇒",
        ">>=": ">>=",
        "forall": "∀",
        "<=": "≤",
        ">=": "≥",
    };

    // Concealable infix elements
    var infixConceals = {
        "intersect": "∩",
        "intersection": "∩",
        "union": "∪",
        "elem": "∈",
        "notElem": "∉",
    };

    // Return the previous CodeMirror token
    function prevToken(editor, token, line) {
        var before = editor.getTokenAt(Pos(line, token.start));
        return before;
    };

    // Return the next CodeMirror token
    function nextToken(editor, token, line) {
        var after = editor.getTokenAt(Pos(line, token.end + 1));
        return after;
    };

    // Create a DOM element for a given conceal element
    function concealDOM(data) {
        var span = document.createElement("span");
        span.innerHTML = data;
        return span;
    }

    // Process a non-infix conceal token.
    function markNonInfixToken(editor, line, token) {
        // We have a special case for the dot operator.  We only want to
        // convert it to a fancy composition if there is a space before it.
        // This preserves things like [1..1000] which CodeMirror parses
        // incorrectly and also lets you write with lenses as record^.a.b.c,
        // which looks better.
        if (token.string == ".") {
            var handle = editor.getLineHandle(line);
            var ch = token.start;
            if (handle.text[ch - 1] != ' ') {
                return false;
            }
        }

        // Check if this is a normal concealable element. (non-infix)
        for (var str in conceals) {
            if (conceals.hasOwnProperty(str)) {
                if (token.string == str) {
                    editor.markText(Pos(line, token.start), Pos(line, token.end), {
                        replacedWith: concealDOM(conceals[str]),
                    });
                    return true;
                }
            }
        }

        return false;
    }

    function markInfixToken(editor, line, prev, token, next) {
        if (prev.string != "`" || next.string != "`") {
            return false;
        }

        for (var str in infixConceals) {
            if (infixConceals.hasOwnProperty(str)) {
                if (token.string == str) {
                    editor.markText(Pos(line, prev.start), Pos(line, next.end), {
                        replacedWith: concealDOM(infixConceals[str]),
                    });
                    return true;
                }
            }
        }

        return true;
    }

    // Mark a token if necessary (mark means change how it looks).
    function markToken(editor, line, token) {
        // If it's a backtick, it might be the end of an infix conceal.
        if (token.string == "`") {
            var prev = prevToken(editor, token, line);
            var prev2 = prevToken(editor, prev, line);
            return markInfixToken(editor, line, prev2, prev, token);
        } 
        // Otherwise, try it as a normal non-infix token
        // Or as the center of an infix token.
        else {
            var marked = markNonInfixToken(editor, line, token);
            if (marked) {
                return true;
            }

            // Try it as the middle of an infix set
            var prev = prevToken(editor, token, line);
            var next = nextToken(editor, token, line);
            return markInfixToken(editor, line, prev, token, next);
        }
    }

    /**
     * Activate conceal in CodeMirror options, don't overwrite other settings
     */
    function concealCell(editor) {
        // Initialize all tokens. Just look at the token at every character.
        editor.eachLine(function (handle) {
            var l = editor.getLineNumber(handle);
            for (var c = 0; c < handle.text.length; c++) {
                var token = editor.getTokenAt(Pos(l, c), true);
                markToken(editor, l, token);
            }
        });

        editor.on("change", function() {
            var cursor = editor.getCursor();
            var token = editor.getTokenAt(cursor, true);
            markToken(editor, cursor.line, token);
        });
    }
    
    /**
     * Add conceal to new cell
     *
     */
    createCell = function (event,nbcell,nbindex) {
        var cell = nbcell.cell;
        if ((cell instanceof IPython.CodeCell)) {
            var editor = cell.code_mirror;
            concealCell(editor)            
        }
    };
    
    /**
    * Add conceal to existing cells
     */
    initExtension = function(event) {
        var cells = IPython.notebook.get_cells();
        for(var i in cells){
            var cell = cells[i];
            if ((cell instanceof IPython.CodeCell)) {
                var editor = cell.code_mirror;
                concealCell(editor);
            }
        }

        $([IPython.events]).on('create.Cell',createCell);
    }

    IPython.concealCell = concealCell;

    require([], initExtension);
})();
