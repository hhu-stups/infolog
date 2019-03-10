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
          this.showSettings();
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
        'infolog:cancelAnalysis': () => this.cancelAnalysis()}),
      atom.commands.add('atom-workspace', {
        'infolog:clearProblems': () => this.clearProblems()}),
      atom.commands.add('atom-workspace', {
        'infolog:toggleShowAllFiles': () => this.toggleShowAllFiles()}),
      atom.commands.add('atom-workspace', {
        'infolog:showSettings': () => this.showSettings()})
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
    if (this.progressPanel.isVisible()) {
      atom.confirm({
        message: "Infolog already running!",
        detail: "There is already an analysis in progress",
        type: "error",
        buttons: ["Abort running analysis", "Cancel"]
      }, (response) => {
        if (response == 0) {
          this.cancelAnalysis();
        }
      });
    }
    const currentEditor = atom.workspace.getActiveTextEditor();
    this.infologClient.onConnect(() => {
      const filePath = currentEditor.getPath();
      this.infologClient.onResponse((response, originFile) => {
        this.linter.updateProblems(response.result.problems, originFile);
        this.infolog.stopInfolog();
      });
      this.infologClient.methodCall("analyzeFile", {
        "path": filePath
      }, filePath);
    });
    this.infolog.startInfolog();
  },

  cancelAnalysis() {
    this.infolog.killInfolog();
  },

  clearProblems() {
    this.linter.clearProblems();
  },

  toggleShowAllFiles() {
    const previousSetting = atom.config.get("linter-ui-default.panelRepresents");
    if (previousSetting == "Entire Project") {
      atom.config.set("linter-ui-default.panelRepresents", "Current File");
    } else {
      atom.config.set("linter-ui-default.panelRepresents", "Entire Project");
    }
  },

  showSettings() {
    atom.workspace.open("atom://config/packages/infolog");
  }
};
