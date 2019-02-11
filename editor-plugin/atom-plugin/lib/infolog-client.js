'use babel';

import { spawn } from 'child_process';
import * as path from 'path';

export default class InfologClient {

  constructor()
  {
      this.port = -1;
  }

  connect(port)
  {
    this.port = port;
    console.log("Client connecting to port", this.port);
  }

  disconnect()
  {
    console.log("Client disconnecting from port", this.port);
    this.port = -1;
  }

}
