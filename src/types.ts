
import { User as ApplicationUser, } from "@prisma/client";
import { Request } from "express";
declare global {
  // eslint-disable-next-line @typescript-eslint/no-namespace
  namespace Express {
    // eslint-disable-next-line @typescript-eslint/no-empty-interface
    interface User extends ApplicationUser {}
  }
}

export type InterpretationRequest = Request<any, any, {
  filepath: string;
}>;