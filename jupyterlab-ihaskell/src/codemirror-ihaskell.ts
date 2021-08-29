import { ICodeMirror, Mode } from '@jupyterlab/codemirror';

export async function defineIHaskellMode({ CodeMirror }: ICodeMirror) {
  await Mode.ensure("haskell");
  await Mode.ensure("r");
  CodeMirror.defineMode("ihaskell", (config) => {
    let hmode = CodeMirror.getMode(config, "haskell");
    return CodeMirror.multiplexingMode(
      hmode,
      {
        // @ts-ignore
        open: /:(?=!)/, // Matches : followed by !, but doesn't consume !
        // @ts-ignore
        close: /^(?!!)/, // Matches start of line not followed by !, doesn't consume character
        mode: CodeMirror.getMode(config, "text/plain"),
        delimStyle: "delimit"
      },
      {
        // @ts-ignore
        open: /\[r\||\[rprint\||\[rgraph\|/,
        // @ts-ignore
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
