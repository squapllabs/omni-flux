import express from 'express';
import authMiddleware from '../../middleware/auth';
import { gstUpdateValidator } from '../../validations/gst';
import { runValidation } from '../../validations/index';
import {
  createGst,
  deleteByGstId,
  getAllGst,
  getByGstId,
  updateGst,
} from '../../controller/gst.controller';
const router = express.Router();

router.post('/', authMiddleware, createGst);

router.put('/', authMiddleware, gstUpdateValidator, runValidation, updateGst);

router.get('/getAll', authMiddleware, getAllGst);

router.get('/get/:gst_id', authMiddleware, getByGstId);

router.delete('/delete/:gst_id', authMiddleware, deleteByGstId);

export default router;
