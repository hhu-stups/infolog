'use babel';

import * as net from 'net'

export default class InfologClient {

  constructor()
  {
      this.port = -1;
      this.messageIDs = 1;
  }

  connect(port)
  {
    console.log("Client connecting to port", port);
    this.client = net.createConnection({ port: port}, () => {
      this.port = port;
      this.lspMethodCall("testmethod", {});
    });
    this.client.on("data", (data) => {
      console.log(`client: ${data}`);
    });
  }

  disconnect()
  {
    console.log("Client disconnecting from port", this.port);
    this.port = -1;
    this.client.destroy();
    this.messageIDs = 1;
  }

  jsonRpcMessage(method, params) {
    return {
      "jsonrpc": "2.0",
      "id": this.messageIDs++,
      "method": method,
      "params": params
    };
  }

  lspMessage(content) {
    var contentLength = new TextEncoder('utf-8').encode(content).length;
    return `Content-Length: ${contentLength}\r\n\r\n` + content;
  }

  lspMethodCall(method, params) {
    this.client.write(this.lspMessage(JSON.stringify(this.jsonRpcMessage(method, params))));
  }

}
