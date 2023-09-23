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
import siteContractor from './siteContractor.route';
import siteExpense from './siteExpense.route';
import warehouse from './warehouse.route';
import warehouseInventory from './warehouseInventory.route';
import upload from './upload.route';
import brand from './brand.route';
import itemType from './itemType.routes';
import masterData from './masterData.route';
import leadEnquiry from './leadEnquiry.route';
import projectWorkbreakDown from './projectWorkbreakDown.route';
import siteExpenseDetails from './siteExpenseDetail.route';
import capability from './capability.route';
import permission from './permissions.route';
import bom from './bomDetail.route';
import vendor from './vendor.route';
import labour from './labour.route';
import machinery from './machinery.route';
import userPrimaryProject from './userPrimaryProject.route';
import projectMemberAssociation from './projectMemberAssociation.route';
import store from './store.route';
import projectSite from './projectSite.route';
import indentRequest from './indentRequest.route';
import inventory from './inventory.route';
import purchaseRequest from './purchaseRequest.route';
import indentRequestDetails from './indentRequestDetails.route';
import purchaseOrder from './purchaseOrder.route';
import purchaseOrderItem from './purchaseOrderItem.route';
import vendorQuotes from './vendorQuotes.route';
import projectInventory from './projectInventory.route';
import stockOutward from './stockOutward.route';
import stockOutwardDetails from './stockOutwardDetails.route';
import stockAudit from './stockAudit.route';
import expense from './expense.route';

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
    path: '/site-contractor',
    route: siteContractor,
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
    path: '/project-workbreak-down',
    route: projectWorkbreakDown,
  },
  {
    path: '/site-expense-details',
    route: siteExpenseDetails,
  },
  {
    path: '/capability',
    route: capability,
  },
  {
    path: '/permission',
    route: permission,
  },
  {
    path: '/bom',
    route: bom,
  },
  {
    path: '/vendor',
    route: vendor,
  },
  {
    path: '/labour',
    route: labour,
  },
  {
    path: '/machinery',
    route: machinery,
  },
  {
    path: '/user-primary-project',
    route: userPrimaryProject,
  },
  {
    path: '/project-member-association',
    route: projectMemberAssociation,
  },
  {
    path: '/store',
    route: store,
  },
  {
    path: '/project-site',
    route: projectSite,
  },
  {
    path: '/indent-request',
    route: indentRequest,
  },
  {
    path: '/inventory',
    route: inventory,
  },
  {
    path: '/purchase-request',
    route: purchaseRequest,
  },
  {
    path: '/indent-request-details',
    route: indentRequestDetails,
  },
  {
    path: '/purchase-order',
    route: purchaseOrder,
  },
  {
    path: '/purchase-order-item',
    route: purchaseOrderItem,
  },
  {
    path: '/vendor-quotes',
    route: vendorQuotes,
  },
  {
    path: '/project-inventory',
    route: projectInventory,
  },
  {
    path: '/stock-outward',
    route: stockOutward,
  },
  {
    path: '/stock-outward-details',
    route: stockOutwardDetails,
  },
  {
    path: '/stock-audit',
    route: stockAudit,
  },
  {
    path: '/expense',
    route: expense,
  },
];

defaultRoutes.forEach((r) => {
  router.use(r.path, r.route);
});

export default router;
