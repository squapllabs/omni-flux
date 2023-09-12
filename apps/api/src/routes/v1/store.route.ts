import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  storeCreateValidator,
  storeUpdateValidator,
} from '../../validations/store';
import { runValidation } from '../../validations/index';
import {
  createStore,
  deleteByStoreId,
  getAllStore,
  getByStoreId,
  updateStore,
} from '../../controller/store.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  storeCreateValidator,
  runValidation,
  createStore
);

router.put(
  '/',
  authMiddleware,
  storeUpdateValidator,
  runValidation,
  updateStore
);

router.get('/get-all', authMiddleware, getAllStore);

router.get('/get/:store_id', authMiddleware, getByStoreId);

router.delete('/delete/:store_id', authMiddleware, deleteByStoreId);

export default router;
