import express from 'express';
import morgan from 'morgan';
import bodyParser from 'body-parser';
import routes from './routes';
import cors from 'cors';

const host = process.env.HOST ?? 'localhost';
const port = process.env.HRMS_API_PORT
  ? Number(process.env.HRMS_API_PORT)
  : 3000;
const app = express();

app.use(morgan('dev'));
app.use(express.static('public'));

/* Parse JSON bodies */
app.use(bodyParser.json());

app.use(express.urlencoded({ extended: false }));
app.use(bodyParser.urlencoded({ extended: false }));

const corsOptions = {
  origin: [`http://${host}:${port}`],
  credentials: true,
  optionsSuccessStatus: 200,
};
app.use(function (req, res, next) {
  res.header('Access-Control-Allow-Credentials', 'true');
  res.header('Access-Control-Allow-Origin', '*');
  res.header(
    'Access-Control-Allow-Headers',
    'Origin, X-Requested-With, Content-Type, Accept,Authorization'
  );
  next();
});

app.use(cors(corsOptions));

app.use('/hrms-api', routes);

app.listen(port, host, () => {
  console.log(`Hrms-api App Listening at http://${host}:${port}`);
});
