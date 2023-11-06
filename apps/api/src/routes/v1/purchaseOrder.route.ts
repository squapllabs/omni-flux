import express from 'express';
import authMiddleware from '../../middleware/auth';
import { purchaseOrderUpdateValidator } from '../../validations/purchaseOrder';
import {
  createPurchaseOrder,
  createPurchaseOrderWithItem,
  deleteByPurchaseOrderId,
  getAllPurchaseOrders,
  getByPurchaseOrderId,
  getByPurchaseRequestId,
  getPOReportData,
  getPOStatistics,
  searchPurchaseOrder,
  updatePurchaseOrder,
  updateStatusAndDocument,
} from '../../controller/purchaseOrder.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/', authMiddleware, createPurchaseOrder);

router.put(
  '/',
  authMiddleware,
  purchaseOrderUpdateValidator,
  runValidation,
  updatePurchaseOrder
);

router.post('/get-all', authMiddleware, getAllPurchaseOrders);

router.get('/get/:purchase_order_id', authMiddleware, getByPurchaseOrderId);

router.delete(
  '/delete/:purchase_order_id',
  authMiddleware,
  deleteByPurchaseOrderId
);

router.post('/search', authMiddleware, searchPurchaseOrder);

router.post(
  '/purchase-order-with-item',
  authMiddleware,
  createPurchaseOrderWithItem
);

router.get(
  '/get-by-purchase-request-id/:purchase_request_id',
  authMiddleware,
  getByPurchaseRequestId
);

router.put(
  '/update-status-and-document',
  authMiddleware,
  purchaseOrderUpdateValidator,
  runValidation,
  updateStatusAndDocument
);

router.get('/get-po-statistics', authMiddleware, getPOStatistics);

router.post('/get-po-report', authMiddleware, getPOReportData);

export default router;
