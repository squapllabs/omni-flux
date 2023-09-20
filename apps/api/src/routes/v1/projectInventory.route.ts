import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  projectInventoryCreateValidator,
  projectInventoryUpdateValidator,
} from '../../validations/projectInventory';
import {
  createProjectInventory,
  deleteByProjectInventoryId,
  getAllProjectInventorys,
  getByProjectInventoryId,
  searchProjectInventory,
  updateProjectInventory,
} from '../../controller/projectInventory.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  projectInventoryCreateValidator,
  runValidation,
  createProjectInventory
);

router.put(
  '/',
  authMiddleware,
  projectInventoryUpdateValidator,
  runValidation,
  updateProjectInventory
);

router.get('/get-all', authMiddleware, getAllProjectInventorys);

router.get(
  '/get/:project_inventory_id',
  authMiddleware,
  getByProjectInventoryId
);

router.delete(
  '/delete/:project_inventory_id',
  authMiddleware,
  deleteByProjectInventoryId
);

router.post('/search', authMiddleware, searchProjectInventory);

export default router;
