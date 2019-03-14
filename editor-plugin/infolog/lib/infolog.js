'use babel';

import InfologServer from './infolog-server'
import InfologClient from './infolog-client'
import InfologLinter from './infolog-linter'
import ProgressView from './progress-view'
import { CompositeDisposable } from 'atom';
import * as path from 'path';
import * as os from 'os';

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
    this.infologClient.disconnect();
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
    }/*,
    proBPath: {
      title: "ProB path",
      description: "Path to ProB directory",
      type: "string",
      default: "TODO"
    },
    proBInfoDir: {
      title: "ProB info directory",
      description: "Path to ProB info directory",
      type: "string",
      default: "TODO"
    },
    proBTargets: {
      title: "ProB targets",
      description: "ProB targets",
      type: "string",
      default: "TODO"
    },*/
  },

  consumeIndie(registerIndie) {
    this.linter = new InfologLinter(registerIndie({
      name: "Infolog"
    }));
  },

  analyzeFile() {
    this._startAnalysis((filePath) => {
      this.infologClient.methodCall("analyzeFile", {
        "path": filePath
      }, filePath);
    });
  },

  analyzeFileProB() {
    let proBPath = atom.config.get("infolog.proBPath");
    let proBInfoDir = atom.config.get("infolog.proBInfoDir");
    let proBTargets = atom.config.get("infolog.proBTargets");
    if (os.type() == "Windows_NT") {
      proBPath = proBPath.replace(/\\/g, "/");
      proBInfoDir = proBInfoDir.replace(/\\/g, "/");
      proBTargets = proBTargets.replace(/\\/g, "/");
    }
    this._startAnalysis((filePath) => {
      this.infologClient.methodCall("analyzeFileProB", {
        "path": filePath,
        "proBPath": proBPath,
        "proBInfoDir": proBInfoDir,
        "proBTargets": proBTargets
      }, filePath);
    });
  },

  _startAnalysis(method) {
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
      method(filePath);
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
