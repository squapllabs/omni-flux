import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  purchaseOrderInvoiceCreateValidator,
  purchaseOrderInvoiceUpdateValidator,
} from '../../validations/purchaseOrderInvoice';
import {
  createPurchaseOrderInvoice,
  deleteByPurchaseOrderInvoiceId,
  getAllPurchaseOrderInvoices,
  getByPurchaseOrderId,
  getByPurchaseOrderInvoiceId,
  searchPurchaseOrderInvoice,
  updatePurchaseOrderInvoice,
} from '../../controller/purchaseOrderInvoice.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  purchaseOrderInvoiceCreateValidator,
  runValidation,
  createPurchaseOrderInvoice
);

router.put(
  '/',
  authMiddleware,
  purchaseOrderInvoiceUpdateValidator,
  runValidation,
  updatePurchaseOrderInvoice
);

router.get('/get-all', authMiddleware, getAllPurchaseOrderInvoices);

router.get(
  '/get/:purchase_order_invoice_id',
  authMiddleware,
  getByPurchaseOrderInvoiceId
);

router.get(
  '/get-by-po-id/:purchase_order_id',
  authMiddleware,
  getByPurchaseOrderId
);

router.delete(
  '/delete/:purchase_order_invoice_id',
  authMiddleware,
  deleteByPurchaseOrderInvoiceId
);

router.post('/search', authMiddleware, searchPurchaseOrderInvoice);

export default router;
