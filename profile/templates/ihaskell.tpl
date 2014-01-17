{%- extends 'full.tpl' -%}


{%- block header -%}
<!DOCTYPE html>
<html>
<head>

<meta charset="utf-8" />
<title>{{resources['metadata']['name']}}</title>

{% for css in resources.inlining.css -%}
    <style type="text/css">
    {{ css }}
    </style>
{% endfor %}

<style type="text/css">
/* Overrides of notebook CSS for static HTML export */
body {
  overflow: visible;
  padding: 8px;
}
.input_area {
  padding: 0.2em;
}

pre {
  padding: 0.2em;
  border: none;
  margin: 0px;
  font-size: 13px;
}
</style>

<!-- Our custom CSS -->
<style type="text/css">
/*
Custom IHaskell CSS.
*/

/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
    display: block;
    padding-bottom: 1.3em;
    padding-left: 0.4em;
}
.hoogle-code {
    display: block;
    font-family: monospace;
    white-space: pre;
}
.hoogle-text {
    display: block;
}
.hoogle-name {
    color: green;
    font-weight: bold;
}
.hoogle-head {
    font-weight: bold;
}
.hoogle-sub {
    display: block;
    margin-left: 0.4em;
}
.hoogle-package {
    font-weight: bold;
    font-style: italic;
}
.hoogle-module {
    font-weight: bold;
}

/* Styles used for basic displays */
.get-type {
    color: green;
    font-weight: bold;
    font-family: monospace;
    display: block;
    white-space: pre;
}

.show-type {
    color: green;
    font-weight: bold;
    font-family: monospace;
    margin-left: 1em;
}

.mono {
    font-family: monospace;
    display: block;
}

.err-msg {
    color: red;
    font-style: italic;
    font-family: monospace;
    white-space: pre;
    display: block;
}

#unshowable {
    color: red;
    font-weight: bold;
}

.err-msg.in.collapse {
  padding-top: 0.7em;
}

/* Code that will get highlighted before it is highlighted */
.highlight-code {
    white-space: pre;
    font-family: monospace;
}

/* Hlint styles */
.suggestion-warning { 
    font-weight: bold;
    color: rgb(200, 130, 0);
}
.suggestion-error { 
    font-weight: bold;
    color: red;
}
.suggestion-name {
    font-weight: bold;
}
</style>

<script src="https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS_HTML" type="text/javascript"></script>
<script type="text/javascript">
init_mathjax = function() {
    if (window.MathJax) {
        // MathJax loaded
        MathJax.Hub.Config({
            tex2jax: {
                inlineMath: [ ['$','$'], ["\\(","\\)"] ],
                displayMath: [ ['$$','$$'], ["\\[","\\]"] ]
            },
            displayAlign: 'left', // Change this to 'center' to center equations.
            "HTML-CSS": {
                styles: {'.MathJax_Display': {"margin": 0}}
            }
        });
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }
}
init_mathjax();
</script>

</head>
{%- endblock header -%}

{% block body %}
<body>
{{ super() }}
</body>
{%- endblock body %}
