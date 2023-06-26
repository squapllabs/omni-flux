import express from 'express';
/* import db from './utils/connection'; */
import bodyParser from 'body-parser';
import routes from './routes/v1';

const host = process.env.HOST ?? 'localhost';
const port = process.env.PORT ? Number(process.env.PORT) : 3000;

const app = express();

/* Parse JSON bodies */
app.use(bodyParser.json());

app.use('/api', routes);

app.listen(port, host, () => {
  console.log(`[ ready ] http://${host}:${port}`);
});
