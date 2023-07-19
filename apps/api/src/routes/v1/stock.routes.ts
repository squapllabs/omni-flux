import express from 'express';
import authMiddleware from '../../middleware/auth';
import { gstUpdateValidator } from '../../validations/gst';
import { runValidation } from '../../validations/index';
import {
  createStock,
  updateStock,
  getAllStock,
  getByStockId,
  deleteByStockId
} from '../../controller/stock.controller';
const router = express.Router();

router.post('/create', authMiddleware, createStock);

router.put('/update', authMiddleware, gstUpdateValidator, runValidation, updateStock);

router.get('/getAll', authMiddleware, getAllStock);

router.get('/get/:stock_id', authMiddleware, getByStockId);

router.delete('/delete/:stock_id', authMiddleware, deleteByStockId);

export default router;