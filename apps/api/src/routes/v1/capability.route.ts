import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  capabilityCreateValidator,
  capabilityUpdateValidator,
} from '../../validations/capability';
import { runValidation } from '../../validations/index';
import {
  createCapability,
  deleteByCapabilityId,
  getAllCapabilities,
  getByCapabilityId,
  searchCapability,
  updateCapability,
} from '../../controller/capability.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  capabilityCreateValidator,
  runValidation,
  createCapability
);

router.put(
  '/',
  authMiddleware,
  capabilityUpdateValidator,
  runValidation,
  updateCapability
);

router.get('/get-all', authMiddleware, getAllCapabilities);

router.get('/get/:capability_id', authMiddleware, getByCapabilityId);

router.delete('/delete/:capability_id', authMiddleware, deleteByCapabilityId);

router.post('/search', authMiddleware, searchCapability);

export default router;
