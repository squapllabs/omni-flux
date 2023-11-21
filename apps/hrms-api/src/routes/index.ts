import express from 'express';
import user from './user.route';

const router = express.Router();

const defaultRoutes = [
  {
    path: '/user',
    route: user,
  },
];

defaultRoutes.forEach((r) => {
  router.use(r.path, r.route);
});

export default router;
