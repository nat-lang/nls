import { User } from "@prisma/client";
import { IVerifyOptions } from "passport-local";

export interface IAuthService {
  authenticate: (
    email: string,
    password: string,
    cb: (error: null, user: User | undefined, options?: IVerifyOptions | undefined) => void
  ) => void;
  createUser: (user: Omit<User, "id">) => Promise<User>;
}
