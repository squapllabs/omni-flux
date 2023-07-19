import express from 'express';
import authMiddleware from '../../middleware/auth';
import { warehouseInventoryUpdateValidator } from '../../validations/warehouseInventory';
import { runValidation } from '../../validations/index';
import {
  createWarehouseInventory,
  deleteByWarehouseInventoryId,
  getAllWarehouseInventory,
  getByWarehouseInventoryId,
  updateWarehouseInventory,
} from '../../controller/warehouseInventory.controller';

const router = express.Router();

router.post('/', authMiddleware, runValidation, createWarehouseInventory);

router.put(
  '/',
  authMiddleware,
  warehouseInventoryUpdateValidator,
  runValidation,
  updateWarehouseInventory
);

router.get('/getAll', authMiddleware, getAllWarehouseInventory);

router.get(
  '/get/:warehouse_inventory_id',
  authMiddleware,
  getByWarehouseInventoryId
);

router.delete(
  '/delete/:warehouse_inventory_id',
  authMiddleware,
  deleteByWarehouseInventoryId
);

export default router;
