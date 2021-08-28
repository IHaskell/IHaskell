// @ts-nocheck
// The types for CodeMirror don't accept a RegExp for 'open' and 'close'

import { ICodeMirror, Mode } from '@jupyterlab/codemirror';

export function defineIHaskellMode({ CodeMirror }: ICodeMirror) {
  Mode.ensure("haskell");
  Mode.ensure("r");
  CodeMirror.defineMode("ihaskell", (config) => {
    let hmode = CodeMirror.getMode(config, "haskell");
    return CodeMirror.multiplexingMode(
      hmode,
      {
        open: /:(?=!)/, // Matches : followed by !, but doesn't consume !
        close: /^(?!!)/, // Matches start of line not followed by !, doesn't consume character
        mode: CodeMirror.getMode(config, "text/plain"),
        delimStyle: "delimit"
      },
      {
        open: /\[r\||\[rprint\||\[rgraph\|/,
        close: /\|\]/,
        mode: CodeMirror.getMode(config, "text/x-rsrc"),
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
}
