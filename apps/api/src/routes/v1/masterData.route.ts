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
  getAllProjectMasterData,
  getByMasterDataId,
  getByParentMasterDataType,
  getByParentType,
  getByProjectId,
  getByProjectIdAndType,
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

router.post('/get-by-parent-type', authMiddleware, getByParentMasterDataType);

router.delete('/delete/:master_data_id', authMiddleware, deleteByMasterDataId);

router.post('/search-master-data', authMiddleware, searchMasterData);

router.get('/get-by-type/:master_data_type', authMiddleware, getByParentType);

router.get('/get-by-project-id/:project_id', authMiddleware, getByProjectId);

router.get(
  '/get-all-project-master-data',
  authMiddleware,
  getAllProjectMasterData
);

router.get(
  '/get-by-project-id-and-type/:project_id/:master_data_type',
  authMiddleware,
  getByProjectIdAndType
);

export default router;
