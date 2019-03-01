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
      path.join(this.infologDirectory, "editor-plugin", "language-server", "infolog-server.pl"),
      "--goal",
      "'infolog-server':start_server,halt."],
      {
        cwd: this.projectDirectory
      });
    this.infolog.stdout.on("data", (data) => {
      if (this.port == -1) {
        this._startup(data);
      }
    });
    this.infolog.stderr.on("data", (data) => {
      const fileNotFoundRegex = /goal:(\s+)ensure_loaded\((.+):(.+)\)/.exec(data);
      if (fileNotFoundRegex) {
        this.callbacks["error"].forEach((callback) => {
          callback(new Error(`Could not find file: ${fileNotFoundRegex[3]}`));
        });
      }
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
