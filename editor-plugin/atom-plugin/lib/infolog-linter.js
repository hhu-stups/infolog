'use babel';

import * as os from 'os'

export default class InfologLinter {
  constructor(linter) {
    this.linter = linter;
    this.clearProblems();
  }

  dispose() {
    this.clearProblems();
    this.linter.dispose();
  }

  updateProblems(newProblems) {
    const existingProblemsWithoutNew = this._existingProblemsFiltered(newProblems);
    this.problems = this._assembleAllProblems(existingProblemsWithoutNew, newProblems);
    console.log("Final problems:", this.problems);
    this._displayProblems();
  }

  clearProblems() {
    this.problems = [];
    this.linter.clearMessages();
  }

  _existingProblemsFiltered(problems) {
    return this.problems.filter((existingProblem) => {
      return !(problems.some((problem) => {
        return existingProblem.File == problem.File;
      }));
    });
  }

  _assembleAllProblems(existingProblemsWithoutNew, problems) {
    return existingProblemsWithoutNew.concat(problems);
  }

  _displayProblems() {
    this.linter.clearMessages();
    console.log("As messages:", this._problemsToMessages());
    this.linter.setAllMessages(this._problemsToMessages());
  }

  _problemsToMessages() {
    return this.problems.map((problem) => {
      return {
        severity: ["error", "warning"].includes(problem.Type) ? problem.Type : "info",
        location: {
          file: this._mapFile(problem),
          position: this._mapPosition(problem.L1, problem.L2)
        },
        excerpt: problem.Category,
        description: this._generateDescription(problem)
      }
    })
  }

  _mapFile(problem) {
    let fileName = problem.File;
    if (os.type() == "Windows_NT") {
      fileName = fileName.replace(/\//g, "\\");
    }

    const openedFilesLowerCased = atom.workspace.getTextEditors().map((editor) => {
      return {
        lowerPath: editor.getPath().toLowerCase(),
        path: editor.getPath()
    };});

    fileName = openedFilesLowerCased.find((file) => {
      return file.lowerPath == fileName;
    });
    if (fileName) {
      return fileName.path;
    } else {
      return "unknown"; // JTODO: return filename of file that triggered the analysis
    }

  }

  _mapPosition(L1, L2) {
    // JTODO spread these ranges over the whole affected line
    if (L1 == "unknown" || L2 == "unknown") {
      return [[0,0], [0,0]];
    } else {
      return [[L1,0], [L2,0]];
    }
  }

  _generateDescription(problem) {
    return "something is wrong with your code!";
  }
}
