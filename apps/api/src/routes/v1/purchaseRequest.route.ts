import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  purchaseRequestCreateValidator,
  purchaseRequestUpdateValidator,
} from '../../validations/purchaseRequest';
import {
  createPurchaseRequest,
  deleteByPurchaseRequestId,
  getAllPurchaseRequests,
  getByPurchaseRequestId,
  searchPurchaseRequest,
  updatePurchaseRequest,
} from '../../controller/purchaseRequest.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  purchaseRequestCreateValidator,
  runValidation,
  createPurchaseRequest
);

router.put(
  '/',
  authMiddleware,
  purchaseRequestUpdateValidator,
  runValidation,
  updatePurchaseRequest
);

router.get('/get-all', authMiddleware, getAllPurchaseRequests);

router.get('/get/:purchase_request_id', authMiddleware, getByPurchaseRequestId);

router.delete(
  '/delete/:purchase_request_id',
  authMiddleware,
  deleteByPurchaseRequestId
);

router.post('/search', authMiddleware, searchPurchaseRequest);

export default router;
