import express from 'express';
import user from './user.route';
import auth from './auth.route';
import role from './role.route';
import gst from './gst.route';
import uom from './uom.route';
import client from './client.route';
import hsnCode from './hsnCode.route';
import category from './category.route';
import subCategory from './subCategory.route';
import subSubCategory from './subSubCategory.route';
import project from './project.route';

const router = express.Router();
const defaultRoutes = [
  {
    path: '/user',
    route: user,
  },
  {
    path: '/auth',
    route: auth,
  },
  {
    path: '/role',
    route: role,
  },
  {
    path: '/gst',
    route: gst,
  },
  {
    path: '/uom',
    route: uom,
  },
  {
    path: '/client',
    route: client,
  },
  {
    path: '/hsn-code',
    route: hsnCode,
  },
  {
    path: '/category',
    route: category,
  },
  {
    path: '/sub-category',
    route: subCategory,
  },
  {
    path: '/sub-sub-category',
    route: subSubCategory,
  },
  {
    path: '/project',
    route: project,
  },
];

defaultRoutes.forEach((r) => {
  router.use(r.path, r.route);
});

export default router;
