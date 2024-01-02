import app from './app';
import env from './env';

if (!env.SERVICE_PORT) throw Error('Port required for local development.');

app.listen(env.SERVICE_PORT);

