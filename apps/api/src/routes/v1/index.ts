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
import projectExpense from './projectExpense.route';
import site from './site.route';
import siteExpense from './siteExpense.route';
import warehouse from './warehouse.route';
import warehouseInventory from './warehouseInventory.route';
import upload from './upload.route';
import brand from './brand.route';
import itemType from './itemType.routes';
import masterData from './masterData.route';
import leadEnquiry from './leadEnquiry.route';
import leadProduct from './leadProduct.route';
import leadTender from './leadTender.route';

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
    path: '/item',
    route: item,
  },
  {
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
    path: '/stock',
    route: stock,
  },
  {
    path: '/upload',
    route: upload,
  },
  {
    path: '/brand',
    route: brand,
  },
  {
    path: '/itemType',
    route: itemType,
  },
  {
    path: '/master-data',
    route: masterData,
  },
  {
    path: '/lead-enquiry',
    route: leadEnquiry,
  },
  {
    path: '/lead-product',
    route: leadProduct,
  },
  {
    path: '/lead-tender',
    route: leadTender,
  },
];

defaultRoutes.forEach((r) => {
  router.use(r.path, r.route);
});

export default router;
