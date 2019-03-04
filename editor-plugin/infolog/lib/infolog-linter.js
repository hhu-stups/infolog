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

  updateProblems(newProblems, fileThatTriggeredAnalysis) {
    newProblems.forEach((problem) => {
      problem.originFile = fileThatTriggeredAnalysis;
    });
    let existingProblemsWithoutNew = this._existingProblemsFiltered(newProblems);
    this.problems = this._assembleAllProblems(existingProblemsWithoutNew, newProblems, fileThatTriggeredAnalysis);
    this.redisplayProblems();
  }

  redisplayProblems() {
    this.linter.clearMessages();
    this.linter.setAllMessages(this._problemsToMessages());
  }

  clearProblems() {
    this.problems = [];
    this.linter.clearMessages();
  }

  _existingProblemsFiltered(problems) {
    return this.problems.filter((existingProblem) => {
      return !(problems.some((problem) => {
        return ((problem.File != "unknown" && existingProblem.File == problem.File)
          || problem.File == "unknown" && existingProblem.originFile == problem.originFile);
      }));
    });
  }

  _assembleAllProblems(existingProblemsWithoutNew, problems, fileThatTriggeredAnalysis) {
    return existingProblemsWithoutNew.concat(problems);
  }

  _problemsToMessages() {
    return this.problems.map((problem) => {
      const [fileName, isUnmapped] = this._mapFile(problem);
      return {
        severity: ["error", "warning"].includes(problem.Type) ? problem.Type : "info",
        location: {
          file: fileName,
          position: this._mapPosition(problem.L1, problem.L2, fileName, isUnmapped)
        },
        excerpt: this._mapExcerpt(problem),
        description: this._generateDescription(problem)
      }
    })
  }

  _mapFile(problem) {
    let fileName = problem.File.toLowerCase();
    if (os.type() == "Windows_NT") {
      fileName = fileName.replace(/\//g, "\\");
    }

    const openedFilesLowerCased = atom.workspace.getTextEditors()
      .filter((editor) => {
        return editor.getPath();
      })
      .map((editor) => {
        return {
          lowerPath: editor.getPath().toLowerCase(),
          path: editor.getPath()
        };
      });

    fileName = openedFilesLowerCased.find((file) => {
      return file.lowerPath == fileName;
    });
    if (problem.File == "unknown") {
      return [problem.originFile, true];
    }
    else if (fileName) {
      return [fileName.path, false];
    } else {
      return [problem.File, true];
    }

  }

  _mapPosition(L1, L2, fileName, isUnmapped) {
    let line1 = L1 - 1, line2 = L2 - 1;
    const problemFile = atom.workspace.getTextEditors().find((editor) => {
      return editor.getPath() == fileName;
    });
    if (L1 == "unknown" || L2 == "unknown" || isUnmapped) {
      line1 = 0;
      line2 = 0;
    }
    if (problemFile) {
      const beginLine1 = this._beginLine(line1, problemFile);
      const endLine2 = this._endLine(line2, problemFile);
      return [[line1,beginLine1],[line2,endLine2]];
    } else {
      return [[line1,0],[line2,0]];
    }
  }

  // determine index of first non-whitespace character of the given line
  _beginLine(lineNumber, file) {
    const line = file.buffer.getLines()[lineNumber];
    const firstNonWhiteChar = line.trim()[0];
    return firstNonWhiteChar ? line.indexOf(firstNonWhiteChar) : 0;
  }

  _endLine(lineNumber, file) {
    const line = file.buffer.getLines()[lineNumber];
    return line.length;
  }

  _mapExcerpt(problem) {
    if (problem.File == "unknown") {
      if (problem.L1 == "unknown" || problem.L2 == "unknown") {
        return `${problem.Category} (from unknown file, lines unknown)`
      } else {
        return `${problem.Category} (from unknown file, line ${problem.L1}-${problem.L2})`;
      }
    } else {
      return problem.Category;
    }
  }

  _generateDescription(problem) {
    return `${problem.Message}\n
      in predicate ${problem.Module}:${problem.Predicate}\n
      Hash: ${problem.Hash}`;
  }
}
