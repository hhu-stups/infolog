'use babel';

import { spawn } from 'child_process';
import * as path from 'path';

export default class InfologServer {

  constructor(
    infologDirectory = "C:\\Users\\Julian\\Documents\\Studium\\Semester\\9\ -\ WS1819\\Bachelorarbeit\\Infolog\\infolog",
    sicstusExecutable = "sicstus")
    {
      this.infologDirectory = infologDirectory;
      this.serverDirectory = path.join(infologDirectory, "editor-plugin", "language-server");
      this.sicstusExecutable = sicstusExecutable;
      this.port = -1;

      this.callbacks = { "start": [], "stop": [] };
    }

  startInfolog()
  {
    this.infolog = spawn(this.sicstusExecutable,
      ['-l',
      path.join(this.serverDirectory, "server.pl"),
      "--goal",
      "start_server,halt."]);
    this.infolog.stdout.on("data", (data) => {
      console.log(`stdout: ${data}`);
      if (this.port == -1) {
        var portRegex = /Listening on port (\w+)/;
        this.port = portRegex.exec(data)[1];
        this.callbacks["start"].forEach((callback) => {
          callback(this.port);
        });
      }
    });
    this.infolog.stderr.on("data", (data) => {
      console.log(`stdout: ${data}`);
    });
    this.infolog.on("close", (code) => {
      console.log(`Process exited with code ${code}`);
    })
  }

  stopInfolog()
  {
    this.callbacks["stop"].forEach((callback) => {
      callback();
    });
    this.infolog.kill();
    this.port = -1;
  }

  onStart(callback)
  {
    this.callbacks["start"].push(callback);
  }

  onStop(callback)
  {
    this.callbacks["stop"].push(callback);
  }

}
