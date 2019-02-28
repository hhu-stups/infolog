'use babel';

import { spawn } from 'child_process';
import * as path from 'path';

export default class InfologServer {

  constructor(
    infologDirectory = atom.config.get("infolog.infologPath"),
    projectDirectory = atom.project.getPaths()[0],
    sicstusExecutable = atom.config.get("infolog.sicstusExecutable"))
    {
      this.infologDirectory = infologDirectory;
      this.serverDirectory = path.join(infologDirectory, "editor-plugin", "language-server");
      this.projectDirectory = projectDirectory || process.cwd();
      this.sicstusExecutable = sicstusExecutable;
      this.port = -1;

      this.callbacks = { "start": [], "stop": [], "error": [] };

      atom.config.onDidChange("infolog.infologPath", (value) => {
        this.infologDirectory = value.newValue;
      });
      atom.config.onDidChange("infolog.sicstusExecutable", (value) => {
        this.sicstusExecutable = value.newValue;
      });
    }

  startInfolog()
  {
    this.infolog = spawn(this.sicstusExecutable,
      ['-l',
      path.join(this.serverDirectory, "infolog-server.pl"),
      "--goal",
      "'infolog-server':start_server,halt."],
      {
        cwd: this.projectDirectory
      });
    this.infolog.stdout.on("data", (data) => {
      console.log(`stdout: ${data}`);
      if (this.port == -1) {
        this._startup(data);
      }
    });
    this.infolog.stderr.on("data", (data) => {
      console.log(`stderr: ${data}`);
    });
    this.infolog.on("close", (code) => {
      console.log(`Process exited with code ${code}`);
    });
    this.infolog.on("error", (error) => {
      this.callbacks["error"].forEach((callback) => {
        callback(error);
      });
      this.killInfolog();
    });
  }

  _startup(data)
  {
    const portRegex = /Listening on port (\w+)/.exec(data);
    if (portRegex) {
      this.port = portRegex[1];
      this.callbacks["start"].forEach((callback) => {
        callback(this.port);
      });
    }
  }

  stopInfolog()
  {
    if (this.port != -1)
    {
      this.callbacks["stop"].forEach((callback) => {
        callback();
      });
    }
    this.port = -1;
  }

  killInfolog()
  {
    this.stopInfolog();
    this.infolog.kill();
  }

  onStart(callback)
  {
    this.callbacks["start"].push(callback);
  }

  onStop(callback)
  {
    this.callbacks["stop"].push(callback);
  }

  onError(callback)
  {
    this.callbacks["error"].push(callback);
  }

}
