import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  purchaseOrderCreateValidator,
  purchaseOrderUpdateValidator,
} from '../../validations/purchaseOrder';
import {
  createPurchaseOrder,
  deleteByPurchaseOrderId,
  getAllPurchaseOrders,
  getByPurchaseOrderId,
  searchPurchaseOrder,
  updatePurchaseOrder,
} from '../../controller/purchaseOrder.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  purchaseOrderCreateValidator,
  runValidation,
  createPurchaseOrder
);

router.put(
  '/',
  authMiddleware,
  purchaseOrderUpdateValidator,
  runValidation,
  updatePurchaseOrder
);

router.get('/get-all', authMiddleware, getAllPurchaseOrders);

router.get('/get/:purchase_order_id', authMiddleware, getByPurchaseOrderId);

router.delete(
  '/delete/:purchase_order_id',
  authMiddleware,
  deleteByPurchaseOrderId
);

router.post('/search', authMiddleware, searchPurchaseOrder);

export default router;
