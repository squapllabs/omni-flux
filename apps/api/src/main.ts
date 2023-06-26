import express from 'express';
/* import db from './utils/connection'; */
import bodyParser from 'body-parser';
import routes from './routes/v1';
import cors from 'cors';
const host = process.env.HOST ?? 'localhost';
const port = process.env.PORT ? Number(process.env.PORT) : 3000;

const app = express();

/* Parse JSON bodies */
app.use(bodyParser.json());


const corsOptions = {
  origin: 'http://localhost:4200',
  credentials: true,
  optionsSuccessStatus: 200,
};
app.use(function (req, res, next) {
  res.header('Access-Control-Allow-Credentials', 'true');
  res.header('Access-Control-Allow-Origin', '*');
  res.header(
    'Access-Control-Allow-Headers',
    'Origin, X-Requested-With, Content-Type, Accept'
  );
  next();
});

app.use('/api', routes);

app.use(cors(corsOptions));

app.listen(port, host, () => {
  console.log(`[ ready ] http://${host}:${port}`);
});
