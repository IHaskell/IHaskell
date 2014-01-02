// leave at least 2 line with only a star on it below, or doc generation fails
/**
 *
 *
 * Placeholder for custom user javascript
 * mainly to be overridden in profile/static/js/custom.js
 * This will always be an empty file in IPython
 *
 * User could add any javascript in the `profile/static/js/custom.js` file
 * (and should create it if it does not exist).
 * It will be executed by the ipython notebook at load time.
 *
 * Same thing with `profile/static/css/custom.css` to inject custom css into the notebook.
 *
 * Example :
 *
 * Create a custom button in toolbar that execute `%qtconsole` in kernel
 * and hence open a qtconsole attached to the same kernel as the current notebook
 *
 *    $([IPython.events]).on('notebook_loaded.Notebook', function(){
 *        IPython.toolbar.add_buttons_group([
 *            {
 *                 'label'   : 'run qtconsole',
 *                 'icon'    : 'ui-icon-calculator', // select your icon from http://jqueryui.com/themeroller/
 *                 'callback': function(){IPython.notebook.kernel.execute('%qtconsole')}
 *            }
 *            // add more button here if needed.
 *            ]);
 *    });
 *
 * Example :
 *
 *  Use `jQuery.getScript(url [, success(script, textStatus, jqXHR)] );`
 *  to load custom script into the notebook.
 *
 *    // to load the metadata ui extension example.
 *    $.getScript('/static/js/celltoolbarpresets/example.js');
 *    // or
 *    // to load the metadata ui extension to control slideshow mode / reveal js for nbconvert
 *    $.getScript('/static/js/celltoolbarpresets/slideshow.js');
 *
 *
 * @module IPython
 * @namespace IPython
 * @class customjs
 * @static
 */

// end of IPython unmodified version 


$([IPython.events]).on('notebook_loaded.Notebook', function(){
    // add here logic that should be run once per **notebook load**
    // (!= page load), like restarting a checkpoint

    var md = IPython.notebook.metadata 
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
            if (c.cell_type === 'code'){
                c.auto_highlight()
            }
        }
    })

    IPython.CodeCell.options_default['cm_config']['mode'] = 'haskell';
    require(['/static/custom/conceal/conceal.js']);
});

var highlightCodes = function() {
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
};

$([IPython.events]).on('shell_reply.Kernel', highlightCodes);
