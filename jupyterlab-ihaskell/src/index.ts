import { JupyterFrontEnd, JupyterFrontEndPlugin } from '@jupyterlab/application';
import { StreamLanguage, LanguageSupport } from '@codemirror/language';
import { IEditorLanguageRegistry } from '@jupyterlab/codemirror';
import { haskell } from '@codemirror/legacy-modes/mode/haskell';

const plugin: JupyterFrontEndPlugin<void> = {
  id: 'ihaskell',
  autoStart: true,
  description: 'A CodeMirror extension for IHaskell',
  requires: [IEditorLanguageRegistry],
  activate: async (app: JupyterFrontEnd, languages: IEditorLanguageRegistry) => {
    registerHaskellLanguage(languages);
  }
};

function registerHaskellLanguage(codemirrorLanguageRegistry: IEditorLanguageRegistry) {
  const languageSupport = new LanguageSupport(StreamLanguage.define(haskell));
  codemirrorLanguageRegistry.addLanguage({
    name: "ihaskell",
    mime: "text/x-ihaskell",
    support: languageSupport,
    extensions: ["hs"],
  });
}

export default plugin;
