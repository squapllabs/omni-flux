import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  hsnCodeCreateValidator,
  hsnCodeUpdateValidator,
} from '../../validations/hsnCode';
import { runValidation } from '../../validations/index';
import {
  addBulkHSNCode,
  addBulkHSNCodeByImport,
  createHsnCode,
  deleteByHsnCodeId,
  getAllHsnCode,
  getByHsnCode,
  getByHsnCodeId,
  updateHsnCode,
} from '../../controller/hsnCode.controller';
import multer from 'multer';
const upload = multer();
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

router.get('/getAll', authMiddleware, getAllHsnCode);

router.get('/get/:hsn_code_id', authMiddleware, getByHsnCodeId);

router.delete('/delete/:hsn_code_id', authMiddleware, deleteByHsnCodeId);

router.get('/getByCode/:code', authMiddleware, getByHsnCode);

router.post(
  '/add-bulk-by-import',
  authMiddleware,
  upload.single('excelFile'),
  addBulkHSNCodeByImport
);

router.post('/add-bulk-hsn-code', authMiddleware, addBulkHSNCode);

export default router;
