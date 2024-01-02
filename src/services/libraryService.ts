import fs from "fs";
import { ILibraryService } from "./interfaces/ILibraryService";
import path from "path";

export class LibraryService implements ILibraryService {
  dir: string;
  suffix: string;

  constructor() {
    this.dir = path.join('/', 'Users', 'alexandershilen', 'natlang', 'modules');
    this.suffix = 'nat';
  }

  write(filename: string, content: string) {
    const file = this.qualify(filename)
    fs.writeFileSync(file, content);
  }
  delete(filename: string) {
    const file = this.qualify(filename);
    fs.unlinkSync(file);
  }
  qualify(filename: string) {
    const prefix = path.join(this.dir, filename);

    return `${prefix}.${this.suffix}`
  }
}