import express from 'express';
import db from './utils/connection'

const host = process.env.HOST ?? 'localhost';
const port = process.env.PORT ? Number(process.env.PORT) : 3000;

const app = express();
import routes from './routes/v1';

app.use('/api',routes);

app.listen(port, host, () => {
  console.log(`[ ready ] http://${host}:${port}`);
});
