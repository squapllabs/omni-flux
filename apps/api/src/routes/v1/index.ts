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
import item from './item.route';
import stock from './stock.route';
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
  {
<<<<<<< HEAD
    path: '/item',
    route: item,
=======
    path: '/project-expense',
    route: projectExpense,
  },
  {
    path: '/site',
    route: site,
  },
  {
    path: '/site-expense',
    route: siteExpense,
  },
  {
    path: '/warehouse',
    route: warehouse,
  },
  {
    path: '/warehouse-inventory',
    route: warehouseInventory,
  },
  {
    path: '/product',
    route: product,
>>>>>>> 9fc8de531c6bc113c3994f837fdb8f54633098f9
  },
  {
    path: '/stock',
    route: stock,
  },
];

defaultRoutes.forEach((r) => {
  router.use(r.path, r.route);
});

export default router;
