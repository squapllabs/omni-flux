import user from '../routes/user.route';

const setupCustomRoutes = (app) => {
  app.use('/api/users', user);
};

export { setupCustomRoutes };
