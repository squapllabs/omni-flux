import express from 'express';
import user  from './user.route';
import auth from './auth.route';

const router = express.Router();
const defaultRoutes = [
    {
      path: '/user',
      route: user
    },
    {
      path:'/auth',
      route: auth
    }
]

defaultRoutes.forEach((r) => {
  router.use(r.path, r.route);
});

export default router;

  