import { ICodeMirror, Mode } from '@jupyterlab/codemirror';

import 'codemirror/lib/codemirror';
import 'codemirror/mode/haskell/haskell';
import 'codemirror/mode/r/r';

export function defineIHaskellMode({ CodeMirror }: ICodeMirror) {
  Mode.ensure("haskell");
  Mode.ensure("r");
  CodeMirror.defineMode("ihaskell", (config) => {
    let hmode = CodeMirror.getMode(config, "haskell");
    return CodeMirror.multiplexingMode(
      hmode,
      {
        open: (/:(?=!)/).toString(), // Matches : followed by !, but doesn't consume !
        close: (/^(?!!)/).toString(), // Matches start of line not followed by !, doesn't consume character
        mode: CodeMirror.getMode(config, "text/plain"),
        delimStyle: "delimit"
      },
      {
        open: (/\[r\||\[rprint\||\[rgraph\|/).toString(),
        close: (/\|\]/).toString(),
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
