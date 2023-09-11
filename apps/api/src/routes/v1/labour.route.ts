import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  labourCreateValidator,
  labourUpdateValidator,
} from '../../validations/labour';
import {
  checkDuplicateLabourType,
  createLabour,
  deleteByLabourId,
  getAllLabours,
  getByLabourId,
  searchLabour,
  updateLabour,
} from '../../controller/labour.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  labourCreateValidator,
  runValidation,
  createLabour
);

router.put(
  '/',
  authMiddleware,
  labourUpdateValidator,
  runValidation,
  updateLabour
);

router.get('/get-all', authMiddleware, getAllLabours);

router.get('/get/:labour_id', authMiddleware, getByLabourId);

router.delete('/delete/:labour_id', authMiddleware, deleteByLabourId);

router.post('/search', authMiddleware, searchLabour);

router.get(
  '/check-duplicate-labour-type/:labour_type',
  authMiddleware,
  checkDuplicateLabourType
);

export default router;
