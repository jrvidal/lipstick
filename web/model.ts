export type CompilationError = [
  msg: string,
  syntax: boolean,
  startLine: number,
  startColumn: number,
  endLine: number,
  endColumn: number
];
