'use babel';

import InfologServer from './infolog-server'
import InfologClient from './infolog-client'
import InfologLinter from './infolog-linter'
import ProgressView from './progress-view'
import { CompositeDisposable } from 'atom';
import * as path from 'path';

export default {

  activate(state) {
    this.infologClient = new InfologClient();
    this.infolog = new InfologServer();
    this.infolog.onStart((port) => {
      this.progressPanel.show();
      this.infologClient.connect(port);
    });
    this.infolog.onStop(() => {
      this.infologClient.disconnect();
      this.progressPanel.hide();
    });
    this.infolog.onError((error) => {
      atom.confirm({
        message: "Could not run Infolog!",
        detail: `Please make sure the plugin is configured correctly.

${error}`,
        type: "error",
        buttons: ["Go to Infolog settings", "Cancel"]
      }, (response) => {
        if (response == 0) {
          atom.workspace.open("atom://config/packages/infolog");
        }
      })
    })

    this.progressView = new ProgressView("Running Infolog...");
    this.progressPanel = atom.workspace.addModalPanel({
      item: this.progressView.getElement(),
      visible: false
    });

    this.subscriptions = new CompositeDisposable();

    // register commands
    this.subscriptions.add(
      atom.commands.add('atom-workspace', {
        'infolog:analyzeFile': () => this.analyzeFile()}),
      atom.commands.add('atom-workspace', {
        'infolog:clearProblems': () => this.clearProblems()})
    );

    // setup open file trigger
    this.subscriptions.add(atom.workspace.observeTextEditors((args) => {
      if (this.linter) {
        this.linter.redisplayProblems();
      }
    }));
  },

  deactivate() {
    this.progressPanel.destroy();
    this.progressView.destroy();
    this.infolog.killInfolog();
    this.infolog.destroy();
    this.infologClient.destroy();
    this.linter.dispose();
    this.subscriptions.dispose();
  },

  serialize() {
    return {};
  },

  config: {
    sicstusExecutable: {
      title: "SICStus path",
      description: "Path to the SICStus executable",
      type: "string",
      default: "sicstus"
    },
    infologPath: {
      title: "Infolog path",
      description: "Path to the directory of the Infolog repository",
      type: "string",
      default: path.normalize(path.join(__dirname, "..", "..", ".."))
    }
  },

  consumeIndie(registerIndie) {
    this.linter = new InfologLinter(registerIndie({
      name: "Infolog"
    }));
  },

  analyzeFile() {
    const currentEditor = atom.workspace.getActiveTextEditor();
    this.infologClient.onConnect(() => {
      const filePath = currentEditor.getPath();
      this.infologClient.onResponse((response, originFile) => {
        this.linter.updateProblems(response.result.problems, originFile);
        this.infolog.stopInfolog();
      });
      this.infologClient.methodCall("analyzeFile", {
        "path": filePath
      });
    });
    this.infolog.startInfolog();
  },

  clearProblems() {
    this.linter.clearProblems();
  }
};
