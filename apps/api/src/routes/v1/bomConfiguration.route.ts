import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  createBomConfiguration,
  updateBomConfiguration,
  getAllBomConfiguration,
  deleteBomConfiguration,
  getByBomConfigurationId,
} from '../../controller/bomConfiguration.controller';

const router = express.Router();

router.post('/', authMiddleware, createBomConfiguration);

router.put('/', authMiddleware, updateBomConfiguration);

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

export default router;
