'use babel';

import * as net from 'net'

export default class InfologClient {

  constructor()
  {
    this.reset();
  }

  reset()
  {
    this.port = -1;
    this.messageIDs = 1;
    this.callbacks = { "connect": [], "disconnect": [], "response": [] };
    this.originFiles = [undefined];
  }

  connect(port)
  {
    this.client = net.createConnection({ port: port}, () => {
      this.port = port;
      this.callbacks["connect"].forEach((callback) => {
        callback();
      });
    });
    this.client.on("data", (data) => {
      this.callbacks["response"].forEach((callback) => {
        const parsedData = JSON.parse(data);
        callback(parsedData, this.originFiles[parsedData.id]);
      });
    });
  }

  disconnect()
  {
    this.callbacks["disconnect"].forEach((callback) => {
      callback();
    });
    this.reset();
    this.client.destroy();
  }

  methodCall(method, params) {
    if (this.port == -1) {
      throw "Cannot call method: Client not connected!"
    }
    this.originFiles.push(params.path);
    const message = this._jsonRpcMessage(method, params);
    this.client.write(this._withHeader(JSON.stringify(message)));
    return message.id;
  }

  onConnect(callback)
  {
    this.callbacks["connect"].push(callback);
  }

  onDisconnect(callback)
  {
    this.callbacks["disconnect"].push(callback);
  }

  onResponse(callback)
  {
    this.callbacks["response"].push(callback);
  }

  _jsonRpcMessage(method, params) {
    return {
      "jsonrpc": "2.0",
      "id": this.messageIDs++,
      "method": method,
      "params": params
    };
  }

  _withHeader(content) {
    const contentLength = new TextEncoder('utf-8').encode(content).length;
    return `Content-Length: ${contentLength}\r\n\r\n` + content;
  }

}
