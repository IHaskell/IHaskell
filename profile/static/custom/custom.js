$([IPython.events]).on('notebook_loaded.Notebook', function(){
    // add here logic that should be run once per **notebook load**
    // (!= page load), like restarting a checkpoint

    var md = IPython.notebook.metadata;
    if(md.language){
        console.log('language already defined and is :', md.language);
    } else {
        md.language = 'haskell' ;
        console.log('add metadata hint that language is haskell...');
    }
});

$([IPython.events]).on('app_initialized.NotebookApp', function(){
    // add here logic that shoudl be run once per **page load**
    // like adding specific UI, or changing the default value
    // of codecell highlight.

    CodeMirror.requireMode('haskell', function(){
        cells = IPython.notebook.get_cells();
        for(var i in cells){
            c = cells[i];
            if (c.cell_type === 'code') {
                // Force the mode to be Haskell
                // This is necessary, otherwise sometimes highlighting just doesn't happen.
                // This may be an IPython bug.
                c.code_mirror.setOption('mode', 'haskell');

                c.auto_highlight()
            }
        }

        // We can only load the conceal scripts once all cells have mode 'haskell'
        require(['/static/custom/conceal/conceal.js']);
    });

    IPython.CodeCell.options_default['cm_config']['mode'] = 'haskell';
});

$([IPython.events]).on('shell_reply.Kernel', function() {
    // Add logic here that should be run once per reply.
    $('.highlight-code').each(function() {
        var $this = $(this),
            $code = $this.html(),
            $unescaped = $('<div/>').html($code).text();
       
        $this.empty();

        // Never highlight this block again.
        this.className = "";
    
        CodeMirror(this, {
                value: $unescaped,
                mode: this.id,
                lineNumbers: false,
                readOnly: true
            });
    });
});
