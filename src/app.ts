import morgan from 'morgan';
import express, { Request, Response, NextFunction } from 'express';
import 'express-async-errors';
import session from "express-session";
import cors from 'cors';
import passport from 'passport';
import HttpStatusCodes, { RouteError } from './http';

import { PrismaClient, User } from '@prisma/client';
import { AuthService } from './services/authService';
import cookieParser from 'cookie-parser';
import { PrismaSessionStore } from '@quixo3/prisma-session-store';
import { InterpretationService } from './services/interpretationService';
import { InterpretationRequest } from './types';
import { NotFoundError } from '@prisma/client/runtime';
import { LibraryService } from './services/libraryService';
import env from './env';

const prisma = new PrismaClient();
const authService = new AuthService(prisma);
const intptService = new InterpretationService();
const libraryService = new LibraryService();
const app = express();

// authentication

passport.use(authService.strategy());

passport.serializeUser((user, cb) => {
  process.nextTick(() => {
    return cb(null, {
      id: user.id,
      email: user.email,
    });
  });
});

passport.deserializeUser(function(user: User, cb) {
  process.nextTick(function() {
    return cb(null, user);
  });
});

// basic middleware

app.use(express.json());
app.use(express.urlencoded({extended: true}));
app.use(morgan('dev'));
console.log(env)
app.use(
  cors({
    origin: env.ALLOWED_ORIGIN,
    credentials: true,
  })
);

// sessions

app.use(cookieParser());
app.use(
  session({
    saveUninitialized: false,
    resave: false,
    unset: 'destroy',
    cookie: {
      secure: process.env.NODE_ENV === 'production',
      httpOnly: false,
      sameSite: process.env.NODE_ENV === 'production' ? "none" : "lax",
      maxAge: 1000 * 60 * 60 * 24,
    },
    secret: '#g34tw4#%RS33rF43yse53a4rf4352^#',
    store: new PrismaSessionStore(
      prisma,
      {
        checkPeriod: 2 * 60 * 1000,
        dbRecordIdIsSessionId: true,
      }
    )
  })
);
app.use(passport.session());

// global error handler

app.use((
  err: Error,
  _req: Request,
  res: Response,
  _next: NextFunction,
) => {

  let status = HttpStatusCodes.BAD_REQUEST;

  if (err instanceof RouteError) {
    status = err.status;
  }
  
  if (err instanceof NotFoundError) {
    status = HttpStatusCodes.NOT_FOUND;
  }

  return res.status(status).json({ error: err.message });
});

// routes

app.post('/interpret/:id', async (req: InterpretationRequest, res) => {
  const file = libraryService.qualify(req.params.id);
  const interpretation = await intptService.interpret(file);

  res.json({ interpretation });
});

app.post('/modules/:id', (req, res) => {
  libraryService.write(req.params.id, req.body.content);
  res.send('Ok');
});

app.delete('/modules/:id', (req, res) => {
  libraryService.delete(req.params.id);
  res.send('Ok');
});

app.post('/login', passport.authenticate('local', { failureMessage: true }), (req, res) => {
  res.json(req.user);
});

app.post('/logout', (req, res, next) => {
  req.logout(err => {
    if (err) return next(err);
    else res.json({});
  });
});

app.get('/me', (req: Request, res: Response) => {
  if (req.user) {
    res.json({ email: req.user.email });
  } else {
    res.json();
  }
});


export default app;
