import express from 'express';
import authMiddleware from '../../middleware/auth';
import { uomCreateValidator, uomUpdateValidator } from '../../validations/uom';
import { runValidation } from '../../validations/index';
import {
  createUom,
  deleteByUomId,
  getAllUom,
  getByUomId,
  getByUomName,
  updateUom,
} from '../../controller/uom.controller';

const router = express.Router();

router.post('/', authMiddleware, uomCreateValidator, runValidation, createUom);

router.put('/', authMiddleware, uomUpdateValidator, runValidation, updateUom);

router.get('/getAll', authMiddleware, getAllUom);

router.get('/get/:uom_id', authMiddleware, getByUomId);

router.delete('/delete/:uom_id', authMiddleware, deleteByUomId);

router.get('/getByName/:name', authMiddleware, getByUomName);

export default router;
