'use babel';

import InfologServer from './infolog-server'
import InfologClient from './infolog-client'
import InfologLinter from './infolog-linter'
import ProgressView from './progress-view'
import { CompositeDisposable } from 'atom';

export default {

  activate(state) {
    this.infologClient = new InfologClient();
    this.infolog = new InfologServer();
    this.infolog.onStart((port) => {
      this.infologClient.connect(port);
    });
    this.infolog.onStop(() => {
      this.infologClient.disconnect();
    });

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

  consumeIndie(registerIndie) {
    this.linter = new InfologLinter(registerIndie({
      name: "Infolog"
    }));
  },

  analyzeFile() {
    this.progressPanel.show();
    const currentEditor = atom.workspace.getActiveTextEditor();
    this.infologClient.onConnect(() => {
      const filePath = currentEditor.getPath();
      this.infologClient.onResponse((response, originFile) => {
        this.linter.updateProblems(response.result.problems, originFile);
        this.infolog.stopInfolog();
        this.progressPanel.hide();
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
