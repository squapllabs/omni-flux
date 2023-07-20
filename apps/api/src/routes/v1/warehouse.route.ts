import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  warehouseCreateValidator,
  warehouseUpdateValidator,
} from '../../validations/warehouse';
import { runValidation } from '../../validations/index';
import {
  createWarehouse,
  deleteByWarehouseId,
  getAllWarehouse,
  getByWarehouseId,
  updateWarehouse,
} from '../../controller/warehouse.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  warehouseCreateValidator,
  runValidation,
  createWarehouse
);

router.put(
  '/',
  authMiddleware,
  warehouseUpdateValidator,
  runValidation,
  updateWarehouse
);

router.get('/getAll', authMiddleware, getAllWarehouse);

router.get('/get/:warehouse_id', authMiddleware, getByWarehouseId);

router.delete('/delete/:warehouse_id', authMiddleware, deleteByWarehouseId);

export default router;
