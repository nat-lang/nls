import { config } from 'dotenv';
import { load } from 'ts-dotenv';

export const loadEnv = (env: string) => {
  const path = `.env.${env}`;

  config({ path });

  return load({
    SERVICE_PORT: {
      type: Number,
      optional: true,
    },
    DATABASE_URL: String,
    ALLOWED_ORIGIN: String
  }, path);
}

const env = loadEnv('production');

export default env;