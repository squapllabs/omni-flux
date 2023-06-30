import express from 'express';
import morgan from 'morgan';
import bodyParser from 'body-parser';
import routes from './routes/v1';
import cors from 'cors';
import jwt from 'jsonwebtoken';


const host = process.env.HOST ?? 'localhost';
const port = process.env.PORT ? Number(process.env.PORT) : 3000;
const app = express();
app.use(morgan('dev'));
app.use(express.static('public'))
/* Parse JSON bodies */
app.use(bodyParser.json());
// const bodyParser = require('body-parser');
app.use(express.urlencoded({ extended: false }));
app.use(bodyParser.urlencoded({ extended: false }));

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
    'Origin, X-Requested-With, Content-Type, Accept,Authorization'
  );
  next();
});
app.use(cors(corsOptions));
/* Token Security Authorization */
app.use(function (req, res, next) {
  const apiCheck = req.rawHeaders
    .map((header) => header.toLowerCase())
    .indexOf('authorization');
  const loginCheck = req.rawHeaders
    .map((header) => header.toLowerCase())
    .indexOf('token');
  const authHeader = req.rawHeaders[apiCheck + 1];
  const str = '/api/user/login';
  const strData = req.url;
  if (strData.includes(str)) {
    if (loginCheck != -1) {
      next();
    } else {
      const result = {
        status: 401,
        message: 'Token is required',
        authendication: 'Unauthorized',
      };
      return res.status(result.status).json(result);
      // return res.sendStatus(401);
    }
  } else if (apiCheck === -1) {
    const result = {
      status: 401,
      message: 'Token is required',
      authendication: 'Unauthorized',
    };
    return res.status(result.status).json(result);
  } else if (authHeader) {
    jwt.verify(authHeader, process.env.API_ACCESS_TOKEN_SECRET_KEY, (err) => {
      if (err) {
        console.log('Secret Token Invalid', err);
        // return res.sendStatus(401);
        const result = {
          status: 401,
          message: 'Secret Token Invalid',
          authendication: 'Unauthorized',
        };
        return res.status(result.status).json(result);
      } else {
        next();
      }
    });
  }
});



app.use('/api', routes);

app.listen(port, host, () => {
  console.log(`[ ready ] http://${host}:${port}`);

});