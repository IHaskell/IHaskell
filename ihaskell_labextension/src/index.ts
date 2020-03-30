import {
  JupyterFrontEnd, JupyterFrontEndPlugin
} from '@jupyterlab/application';

import './codemirror-ihaskell';

import '../style/index.css';

/**
 * Initialization data for the extension1 extension.
 */
const extension: JupyterFrontEndPlugin<void> = {
  id: 'ihaskell',
  autoStart: true,
  requires: [],
  activate: (app: JupyterFrontEnd) =>
  {
    app.serviceManager.ready
      .then(() => {defineIHaskell()});
  }
};

function defineIHaskell() {
  console.log('ihaskell codemirror activated');
}


export default extension;
