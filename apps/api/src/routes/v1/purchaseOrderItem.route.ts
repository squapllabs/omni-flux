import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  purchaseOrderItemCreateValidator,
  purchaseOrderItemUpdateValidator,
} from '../../validations/purchaseOrderItem';
import {
  createPurchaseOrderItem,
  deleteByPurchaseOrderItemId,
  getAllPurchaseOrderItems,
  getByPurchaseOrderItemId,
  searchPurchaseOrderItem,
  updatePurchaseOrderItem,
} from '../../controller/purchaseOrderItem.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  purchaseOrderItemCreateValidator,
  runValidation,
  createPurchaseOrderItem
);

router.put(
  '/',
  authMiddleware,
  purchaseOrderItemUpdateValidator,
  runValidation,
  updatePurchaseOrderItem
);

router.get('/get-all', authMiddleware, getAllPurchaseOrderItems);

router.get(
  '/get/:purchase_order_item_id',
  authMiddleware,
  getByPurchaseOrderItemId
);

router.delete(
  '/delete/:purchase_order_item_id',
  authMiddleware,
  deleteByPurchaseOrderItemId
);

router.post('/search', authMiddleware, searchPurchaseOrderItem);

export default router;
