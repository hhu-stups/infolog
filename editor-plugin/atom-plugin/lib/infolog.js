'use babel';

import InfologView from './infolog-view';
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

    // Events subscribed to in atom's system can be easily cleaned up with a CompositeDisposable
    this.subscriptions = new CompositeDisposable();

    // Register command that toggles this view
    this.subscriptions.add(atom.commands.add('atom-workspace', {
      'infolog:toggle': () => this.toggle()
    }));
  },

  deactivate() {
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
    console.log('Infolog was toggled!');
    return (
      this.modalPanel.isVisible() ?
      this.modalPanel.hide() :
      this.modalPanel.show()
    );
  }

};
