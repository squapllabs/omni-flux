import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  leadProductCreateValidator,
  leadProductUpdateValidator,
} from '../../validations/leadProduct';
import { runValidation } from '../../validations/index';
import {
  createLeadProduct,
  deleteByLeadProductId,
  getAllLeadProduct,
  getByLeadProductId,
  updateLeadProduct,
} from '../../controller/leadProduct.controller';
const router = express.Router();

router.post(
  '/',
  authMiddleware,
  leadProductCreateValidator,
  runValidation,
  createLeadProduct
);

router.put(
  '/',
  authMiddleware,
  leadProductUpdateValidator,
  runValidation,
  updateLeadProduct
);

router.get('/get-all', authMiddleware, getAllLeadProduct);

router.get('/get/:lead_product_id', authMiddleware, getByLeadProductId);

router.delete(
  '/delete/:lead_product_id',
  authMiddleware,
  deleteByLeadProductId
);

export default router;
