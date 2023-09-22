import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  stockOutwardCreateValidator,
  stockOutwardUpdateValidator,
} from '../../validations/stockOutward';
import {
  createStockOutward,
  deleteByStockOutwardId,
  getAllStockOutwards,
  getByProjectId,
  getByStockOutwardId,
  searchStockOutward,
  updateStockOutward,
} from '../../controller/stockOutward.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  stockOutwardCreateValidator,
  runValidation,
  createStockOutward
);

router.put(
  '/',
  authMiddleware,
  stockOutwardUpdateValidator,
  runValidation,
  updateStockOutward
);

router.get('/get-all', authMiddleware, getAllStockOutwards);

router.get('/get/:stock_outward_id', authMiddleware, getByStockOutwardId);

router.delete(
  '/delete/:stock_outward_id',
  authMiddleware,
  deleteByStockOutwardId
);

router.post('/search', authMiddleware, searchStockOutward);

router.get('/get-by-project-id/:project_id', authMiddleware, getByProjectId);

export default router;
