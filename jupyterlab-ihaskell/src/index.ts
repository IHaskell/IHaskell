import {
  JupyterFrontEnd, JupyterFrontEndPlugin
} from '@jupyterlab/application';
import { StreamLanguage, LanguageSupport } from '@codemirror/language';
import {
  EditorLanguageRegistry,
  IEditorLanguageRegistry,
} from '@jupyterlab/codemirror';

export const languagePlugin: JupyterFrontEndPlugin<IEditorLanguageRegistry> = {
  id: '@jupyterlab/codemirror-extension:language-ihaskell',
  description: 'A CodeMirror extension for IHaskell',
  requires: [IEditorLanguageRegistry],
  activate: (app: JupyterFrontEnd) => {
    const languages = new EditorLanguageRegistry();

    languages.addLanguage({
      name: 'ihaskell',
      mime: 'text/x-ihaskell',
      load: async () => {
        const hs = await Promise.resolve(import('@codemirror/legacy-modes/mode/haskell'));
        const parser = StreamLanguage.define(hs.haskell);
        const languageSupport = new LanguageSupport(parser);
        return languageSupport;
      }
    });
    return languages;
  }
};
