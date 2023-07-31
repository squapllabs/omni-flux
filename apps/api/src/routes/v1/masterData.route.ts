import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  masterDataCreateValidator,
  masterDataUpdateValidator,
} from '../../validations/masterData';
import {
  createMasterData,
  deleteByMasterDataId,
  getAllMasterData,
  getAllParentMasterData,
  getByMasterDataId,
  getByParentMasterDataType,
  searchMasterData,
  updateMasterData,
} from '../../controller/masterData.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  masterDataCreateValidator,
  runValidation,
  createMasterData
);

router.put(
  '/',
  authMiddleware,
  masterDataUpdateValidator,
  runValidation,
  updateMasterData
);

router.get('/get-all', authMiddleware, getAllMasterData);

router.get(
  '/get-all-parent-master-data',
  authMiddleware,
  getAllParentMasterData
);

router.get('/get/:master_data_id', authMiddleware, getByMasterDataId);

router.get(
  '/get-by-parent-type/:master_data_type/:parent_master_data_id?',
  authMiddleware,
  getByParentMasterDataType
);

router.delete('/delete/:master_data_id', authMiddleware, deleteByMasterDataId);

router.post('/search-master-data', authMiddleware, searchMasterData);

export default router;
