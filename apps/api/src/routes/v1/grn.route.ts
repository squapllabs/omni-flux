import express from 'express';
import authMiddleware from '../../middleware/auth';
import { grnCreateValidator } from '../../validations/grn';
import {
  createGrn,
  getAllGrns,
  getByGrnId,
  getByPurchaseOrderId,
  searchGrn,
} from '../../controller/grn.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/', authMiddleware, grnCreateValidator, runValidation, createGrn);

router.get('/get-all', authMiddleware, getAllGrns);

router.get('/get/:grn_id', authMiddleware, getByGrnId);

router.post('/search', authMiddleware, searchGrn);

router.get(
  '/get-by-purchase-order-id/:purchase_order_id',
  authMiddleware,
  getByPurchaseOrderId
);

export default router;
