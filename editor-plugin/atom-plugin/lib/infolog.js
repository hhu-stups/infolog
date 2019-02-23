'use babel';

import InfologView from './infolog-view';
import InfologServer from './infolog-server'
import InfologClient from './infolog-client'
import { CompositeDisposable } from 'atom';

export default {

  infologView: null,
  modalPanel: null,
  subscriptions: null,

  activate(state) {
    this.infologView = new InfologView(state.infologViewState);
    this.modalPanel = atom.workspace.addModalPanel({
      item: this.infologView.getElement(),
      visible: false
    });

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
        'infolog:toggle': () => this.toggle()}),
      atom.commands.add('atom-workspace', {
        'infolog:analyzeFile': () => this.analyzeFile()})
  );
  },

  deactivate() {
    this.infolog.killInfolog();
    this.infolog.destroy();
    this.infologClient.destroy();
    this.modalPanel.destroy();
    this.subscriptions.dispose();
    this.infologView.destroy();
  },

  serialize() {
    return {
      infologViewState: this.infologView.serialize()
    };
  },

  toggle() {
    this.modalPanel.isVisible() ?
    this.infolog.stopInfolog() :
    this.infolog.startInfolog();
    return (
      this.modalPanel.isVisible() ?
      this.modalPanel.hide() :
      this.modalPanel.show()
    );
  },

  analyzeFile() {
    this.infologClient.onConnect(() => {
      var filePath = atom.workspace.getActivePaneItem().buffer.file.path;
      this.infologClient.methodCall("analyzeFile", {
        "path": filePath
      });
    });
    this.infologClient.onResponse((response) => {
      console.log("Received reply to", response.id);
      this.infolog.stopInfolog();
    });
    this.infolog.startInfolog();
  }

};
