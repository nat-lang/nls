import { PrismaClient, User } from "@prisma/client";

import * as crypto from "crypto";
import { Request, Response, NextFunction } from 'express';
import { IVerifyOptions, Strategy } from "passport-local";
import { IAuthService } from "./interfaces/IAuthService";
import HttpStatusCodes from "@src/http";

export class AuthService implements IAuthService {
  client: PrismaClient;

  constructor(client: PrismaClient) {
    this.client = client;
  }

  strategy() {
    return new Strategy(this.authenticate.bind(this));
  }

  async authenticate(
    email: string,
    password: string,
    cb: (error: null, user: User | undefined, options?: IVerifyOptions | undefined) => void
  ) {
    const user = await this.client.user.findFirst({ where: { email } });

    if (user) {
      const equal = this.comparePassword(user.password, password);

      if (equal) return cb(null, user);
    }
  
    return cb(null, undefined, { message: 'Incorrect username or password.' });
  }
  
  async createUser({ password, ...user }: Omit<User, "id">) {
    return this.client.user.create({
      data: {
        password: this.hashPassword(password),
        ...user
      }
    })
  }

  requireAuth(req: Request, res: Response, next: NextFunction) {
    if (req.isAuthenticated()) return next();

    res.status(HttpStatusCodes.FORBIDDEN);
    res.json();
}

  hashPassword(password: string) {
    const salt = crypto.randomBytes(16).toString("hex");
    const buf = crypto.scryptSync(password, salt, 64);

    return `${buf.toString("hex")}.${salt}`;
  }

  comparePassword(
    storedPassword: string,
    suppliedPassword: string
  ): boolean {
    const [hashedPassword, salt] = storedPassword.split(".");

    const hashedPasswordBuf = Buffer.from(hashedPassword, "hex");
    const suppliedPasswordBuf = crypto.scryptSync(suppliedPassword, salt, 64);

    return crypto.timingSafeEqual(hashedPasswordBuf, suppliedPasswordBuf);
  }
}