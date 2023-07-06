import jwt from 'jsonwebtoken';

const authMiddleware = async (req, res, next) => {
  const tokenIndex = req.rawHeaders
    .map((header) => header.toLowerCase())
    .indexOf('authorization');

  const token = req.rawHeaders[tokenIndex + 1];

  if (tokenIndex === -1) {
    const result = {
      status: 401,
      message: 'Token is required',
      authendication: 'Unauthorized',
    };
    return res.status(result.status).json(result);
  } else if (token) {
    const bearerToken = token;
    const originalToken = bearerToken.replace('Bearer ', '');

    jwt.verify(
      originalToken,
      process.env.API_ACCESS_TOKEN_SECRET_KEY,
      (err) => {
        if (err) {
          console.log('Secret Token Invalid', err);

          const result = {
            status: 401,
            message: 'Secret Token Invalid',
            authendication: 'Unauthorized',
          };

          return res.status(result.status).json(result);
        } else {
          next();
        }
      }
    );
  }
};

export default authMiddleware;
