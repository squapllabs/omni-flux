import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  hsnCodeCreateValidator,
  hsnCodeUpdateValidator,
} from '../../validations/hsnCode';
import { runValidation } from '../../validations/index';
import {
  createHsnCode,
  deleteByHsnCodeId,
  getAllHsnCode,
  getByHsnCodeId,
  updateHsnCode,
} from '../../controller/hsnCode.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  hsnCodeCreateValidator,
  runValidation,
  createHsnCode
);

router.put(
  '/',
  authMiddleware,
  hsnCodeUpdateValidator,
  runValidation,
  updateHsnCode
);

router.get('/getAll', getAllHsnCode);

router.get('/get/:hsn_code_id', authMiddleware, getByHsnCodeId);

router.delete('/delete/:hsn_code_id', authMiddleware, deleteByHsnCodeId);

export default router;
