import { IInterpretationService } from "./interfaces/IInterpretationService";
import { exec } from "node:child_process";
import util from "util";

export class InterpretationService implements IInterpretationService {
  clientPath;
  clientOpts;

  constructor() {
    this.clientPath = "/usr/local/bin/nat";
    this.clientOpts = ``;
  }

  async interpret(filepath: string) {
    const { stdout } = await util.promisify(exec)(`${this.clientPath} ${filepath}`);

    return stdout;
  }
}
