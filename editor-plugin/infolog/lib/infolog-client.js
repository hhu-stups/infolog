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
    this.triggers = { "connect": [], "disconnect": [], "response": [] };
    this.originFiles = [undefined];
    this.responseBuffer = [];
  }

  connect(port)
  {
    this.client = net.createConnection({ port: port}, () => {
      this.port = port;
      this.triggers["connect"].forEach((callback) => {
        callback();
      });
    });
    this.client.on("data", (data) => {
      this.triggers["response"].forEach((callback) => {
        this.responseBuffer.push(data);
        const message = Buffer.concat(this.responseBuffer);
        try {
          const parsedData = JSON.parse(message);
          callback(parsedData, this.originFiles[parsedData.id]);
        } finally {
          // ignore incomplete message
        }
      });
    });
  }

  disconnect()
  {
    this.triggers["disconnect"].forEach((callback) => {
      callback();
    });
    this.reset();
    if (this.client) {
      this.client.destroy();
    }
  }

  methodCall(method, params, originFile) {
    if (this.port == -1) {
      throw "Cannot call method: Client not connected!"
    }
    this.originFiles.push(originFile);
    const message = this._jsonRpcMessage(method, params);
    this.client.write(this._withHeader(JSON.stringify(message)));
    return message.id;
  }

  onConnect(callback)
  {
    this.triggers["connect"].push(callback);
  }

  onDisconnect(callback)
  {
    this.triggers["disconnect"].push(callback);
  }

  onResponse(callback)
  {
    this.triggers["response"].push(callback);
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
