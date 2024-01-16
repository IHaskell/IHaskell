import { JupyterFrontEnd, JupyterFrontEndPlugin } from '@jupyterlab/application';
import { StreamLanguage, LanguageSupport } from '@codemirror/language';
import { IEditorLanguageRegistry } from '@jupyterlab/codemirror';
import { haskell } from '@codemirror/legacy-modes/mode/haskell';

export const languagePlugin: JupyterFrontEndPlugin<void> = {
  id: '@jupyterlab/codemirror-extension:language-ihaskell',
  description: 'A CodeMirror extension for IHaskell',
  requires: [IEditorLanguageRegistry],
  activate: (app: JupyterFrontEnd, languages: IEditorLanguageRegistry) => {
    languages.addLanguage({
      name: 'ihaskell',
      mime: 'text/x-haskell',
      load: async () => {
        const lang = StreamLanguage.define(haskell);
        const languageSupport = new LanguageSupport(lang);
        return Promise.resolve(languageSupport);
      }
    });
  }
};
