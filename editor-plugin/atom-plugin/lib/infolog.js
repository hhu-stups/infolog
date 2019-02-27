'use babel';

import InfologServer from './infolog-server'
import InfologClient from './infolog-client'
import InfologLinter from './infolog-linter'
import { CompositeDisposable } from 'atom';

export default {

  infologView: null,
  modalPanel: null,
  subscriptions: null,

  activate(state) {
    this.infologClient = new InfologClient();
    this.infolog = new InfologServer();
    this.infolog.onStart((port) => {
      this.infologClient.connect(port);
    });
    this.infolog.onStop(() => {
      this.infologClient.disconnect();
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
    this.infologClient.onConnect(() => {
      const filePath = atom.workspace.getActiveTextEditor().getPath();
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
