import express from 'express';
import { setupLogging } from './middlewares/logging';
import { setupProxies } from './middlewares/proxy';
import { routes } from './routes/index';
import { setupCustomRoutes } from './middlewares/customRoutes';
import { setupAuth } from './middlewares/auth';
import bodyParser from 'body-parser';
import cors from 'cors';

const app = express();
app.use(bodyParser.json());

const host = process.env.HOST ?? 'localhost';
const port = process.env.GATEWAY_PORT ? Number(process.env.GATEWAY_PORT) : 3000;

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

/* Logging middleware */

setupLogging(app);
setupAuth(app, routes);
setupCustomRoutes(app);
setupProxies(app, routes);

const server = app.listen(port, () => {
  console.log(`Gateway App Listening at http://localhost:${port}`);
});
server.on('error', console.error);
