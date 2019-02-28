'use babel';

export default class ProgressView {

  constructor(progressMessage) {
    this.element = document.createElement('div');
    this.element.classList.add('infolog');

    const message = document.createElement('div');
    message.textContent = progressMessage;
    message.classList.add('message');
    this.element.appendChild(message);
  }

  serialize() {}

  destroy() {
    this.element.remove();
  }

  getElement() {
    return this.element;
  }

}
