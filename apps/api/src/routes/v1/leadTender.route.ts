import express from 'express';
import authMiddleware from '../../middleware/auth';
import { leadTenderUpdateValidator } from '../../validations/leadTender';
import { runValidation } from '../../validations/index';
import {
  createLeadTender,
  deleteByLeadTenderId,
  getAllLeadTender,
  getByLeadTenderId,
  updateLeadTender,
} from '../../controller/leadTender.controller';
const router = express.Router();

router.post('/', authMiddleware, createLeadTender);

router.put(
  '/',
  authMiddleware,
  leadTenderUpdateValidator,
  runValidation,
  updateLeadTender
);

router.get('/get-all', authMiddleware, getAllLeadTender);

router.get('/get/:lead_tender_id', authMiddleware, getByLeadTenderId);

router.delete('/delete/:lead_tender_id', authMiddleware, deleteByLeadTenderId);

export default router;
