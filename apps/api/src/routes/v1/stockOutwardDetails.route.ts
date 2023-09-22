import express from 'express';
import authMiddleware from '../../middleware/auth';
import { getByStockOutwardId } from '../../controller/stockOutwardDetails.controller';

const router = express.Router();

router.get(
  '/get-by-stock-outward-id/:stock_outward_id',
  authMiddleware,
  getByStockOutwardId
);

export default router;
