import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  inventoryCreateValidator,
  inventoryUpdateValidator,
} from '../../validations/inventory';
import {
  createInventory,
  deleteByInventoryId,
  getAllInventorys,
  getByInventoryId,
  searchInventory,
  updateInventory,
} from '../../controller/inventory.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  inventoryCreateValidator,
  runValidation,
  createInventory
);

router.put(
  '/',
  authMiddleware,
  inventoryUpdateValidator,
  runValidation,
  updateInventory
);

router.get('/get-all', authMiddleware, getAllInventorys);

router.get('/get/:inventory_id', authMiddleware, getByInventoryId);

router.delete('/delete/:inventory_id', authMiddleware, deleteByInventoryId);

router.post('/search', authMiddleware, searchInventory);

export default router;
