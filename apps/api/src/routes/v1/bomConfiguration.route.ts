import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  createBomConfiguration,
  updateBomConfiguration,
  getAllBomConfiguration,
  deleteBomConfiguration,
  getByBomConfigurationId,
  searchBomConfiguration,
} from '../../controller/bomConfiguration.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/', authMiddleware, runValidation, createBomConfiguration);

router.put('/', authMiddleware, runValidation, updateBomConfiguration);

router.get('/get-all', authMiddleware, getAllBomConfiguration);

router.delete(
  '/delete/:bom_configuration_id',
  authMiddleware,
  deleteBomConfiguration
);

router.get(
  '/get-id/:bom_configuration_id',
  authMiddleware,
  getByBomConfigurationId
);

router.post('/search', authMiddleware, searchBomConfiguration);

export default router;
