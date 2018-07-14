import * as CodeMirror from 'codemirror';

import 'codemirror/lib/codemirror';
import 'codemirror/mode/haskell/haskell';

CodeMirror.defineMode("ihaskell", (config) => {
  let hmode = CodeMirror.getMode(config, "haskell");
    return CodeMirror.multiplexingMode(
        hmode,
        {
           open: /:(?=!)/, // Matches : followed by !, but doesn't consume !
           close: /^(?!!)/, // Matches start of line not followed by !, doesn't consume character
           mode: CodeMirror.getMode(config, "text/plain"),
           delimStyle: "delimit"
        }
        );
});

CodeMirror.defineMIME("text/x-ihaskell", "ihaskell");

CodeMirror.modeInfo.push({
  ext: ['hs'],
  mime: "text/x-ihaskell",
  mode: 'ihaskell',
  name: 'ihaskell'
});
