// This is an extension that enables hiding input cells.  It adds a button to
// the cell toolbars to hide and unhide cells, as well as command-mode
// keybindings to left and right arrow keys. Whether or not a cell is hidden is
// stored in the metadata and thus is saved in the notebook. A custom template
// which checks for the "hidden" field in cell metadata could be used to have
// nbconvert ignore hidden cells.
"using strict";
var hideInputCellExtension = (function(){
    var Pos = CodeMirror.Pos;

    // What text to show for hidden cells.  This has to be created every time,
    // otherwise you wouldn't be able to hide more than one cell.
    var createHiding = function() {
        var hiding = document.createElement("span");
        hiding.innerHTML = "…";
        return hiding;
    }

    // UI Generator for a simple toggle button.  The model for this code is
    // taken from IPython.CellToolbar.utils.checkbox_ui_Generator.
    IPython.CellToolbar.utils.button_ui_generator = function(name, handler, textfun){
        return function(div, cell, celltoolbar) {
            var button_container = $(div);
            var initText = textfun(cell);

            var button = $('<input/>').attr('type', 'button')
                                      .attr('value', initText)
                                      .css('height', '1.1em')
                                      .css('font-size', 20);
            var lbl = $('<label/>').append($('<span/>').text(name));
            lbl.append(button);
            button.click(function() { 
                handler(cell);
                var newText = textfun(cell);
                button.attr('value', newText);
            });
            cell.hide_button = button;
            cell.button_container = button_container;
            button_container.append($('<div/>').append(lbl));
        };
    };

    // Ensure a cell has the metadata object. Sometimes they don't for unknown reasons.
    // Might have something to do with ordering of cell initialization, so this is a hack.
    var requireMetadata = function(cell) {
        if(cell.metadata === undefined) {
            cell.metadata = {};
            cell.metadata.hidden = false;
        }
    }

    // Return the text to show in the button for this cell.
    var textToShow = function(cell) {
        // What text to show on buttons when concealed or shown.
        var concealedButton = "⇦";
        var shownButton = "⇩";

        requireMetadata(cell);

        if(cell.metadata.hidden) {
            return concealedButton;
        } else {
            return shownButton;
        }
    };

    // Update whether a cell is visible.
    var updateCellVisibility = function(cell, visible) {
        cell.metadata.hidden = visible;
        if(cell.metadata.hidden) {
            if (cell.mark === undefined) {
                var editor = cell.code_mirror;
                var nLines = editor.lineCount();
                var firstLineLen = editor.getLine(0).length;
                var lastLineLen = editor.getLine(nLines - 1).length;
                var mark = editor.markText(Pos(0, firstLineLen), Pos(nLines, lastLineLen + 1), {
                    replacedWith: createHiding(),
                });
                cell.mark = mark;
            }
        } else if (cell.mark !== undefined) {
            cell.mark.clear();
            cell.mark = undefined;
        }

        cell.hide_button.attr('value', textToShow(cell));
    }

    // Create and register the method that creates the hide arrow.
    var flag_name = 'hide_input';
    var cell_flag_init = IPython.CellToolbar.utils.button_ui_generator("", function(cell) {
        // Toggle cell visibility.
        updateCellVisibility(cell, !cell.metadata.hidden);
    }, textToShow);
    IPython.CellToolbar.register_callback(flag_name, cell_flag_init);


    // Create and register the toolbar with IPython.
    IPython.CellToolbar.register_preset('Hiding', [flag_name]);

    var updateCellToolbar = function(cell) {
        var type = cell.cell_type;
        if(type != 'code') {
            // Set cell to visible.
            updateCellVisibility(cell, false);

            // Hide the toolbar on Markdown and other non-code cells.
            cell.celltoolbar.hide();
        } else {
            // Show toolbar on code cells.
            cell.celltoolbar.show();
        }
    };

    var initExtension = function(event) {
        IPython.CellToolbar.activate_preset("Hiding");

        IPython.keyboard_manager.command_shortcuts.add_shortcuts({
            "left": {
                help: "Hide an input cell.",
                help_index: "zz",
                handler: function(event) {
                    var cell = IPython.notebook.get_selected_cell();
                    updateCellVisibility(cell, true);
                }
            },
            "right": {
                help: "Unhide an input cell.",
                help_index: "zz",
                handler: function(event) {
                    var cell = IPython.notebook.get_selected_cell();
                    updateCellVisibility(cell, false);
                }
            }
        });

        var cells = IPython.notebook.get_cells();
        for(var i in cells){
            var cell = cells[i];
            if ((cell instanceof IPython.CodeCell)) {
                updateCellVisibility(cell);
            }
            updateCellToolbar(cell);
        }

        $([IPython.events]).on('create.Cell', requireMetadata);
    }

    // When enetering edit mode, unhide the current cell so you can edit it.
    $([IPython.events]).on('edit_mode.Cell',function () {
        var cell = IPython.notebook.get_selected_cell();
        if(cell.cell_type != "markdown") {
            updateCellVisibility(cell, false);
        }
    });
    

    require([], initExtension);

    $([IPython.events]).on('selected_cell_type_changed.Notebook', function (event, data) {
        var cell = IPython.notebook.get_selected_cell();
        updateCellToolbar(cell);
    });

    console.log("Loaded input cell hiding extension.")
})();
