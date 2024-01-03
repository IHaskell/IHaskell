import { JupyterFrontEnd, JupyterFrontEndPlugin } from '@jupyterlab/application';
import { StreamLanguage, LanguageSupport } from '@codemirror/language';
import { IEditorLanguageRegistry } from '@jupyterlab/codemirror';

export const languagePlugin: JupyterFrontEndPlugin<void> = {
  id: '@jupyterlab/codemirror-extension:language-ihaskell',
  description: 'A CodeMirror extension for IHaskell',
  requires: [IEditorLanguageRegistry],
  activate: (app: JupyterFrontEnd, languages: IEditorLanguageRegistry) => {
    languages.addLanguage({
      name: 'ihaskell',
      mime: 'text/x-ihaskell',
      load: async () => {
        const hs = await import('@codemirror/legacy-modes/mode/haskell');
        const parser = StreamLanguage.define(hs.haskell);
        const languageSupport = new LanguageSupport(parser);
        return Promise.resolve(languageSupport);
      }
    });
  }
};
