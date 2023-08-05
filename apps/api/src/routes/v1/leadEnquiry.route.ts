import express from 'express';
import authMiddleware from '../../middleware/auth';
import { leadEnquiryUpdateValidator } from '../../validations/leadEnquiry';
import { runValidation } from '../../validations/index';
import {
  createLeadEnquiry,
  deleteByLeadEnquiryId,
  getAllLeadEnquiry,
  getByLeadEnquiryId,
  searchLeadEnquiry,
  updateLeadEnquiry,
} from '../../controller/leadEnquiry.controller';

const router = express.Router();

router.post('/', authMiddleware, createLeadEnquiry);

router.put(
  '/',
  authMiddleware,
  leadEnquiryUpdateValidator,
  runValidation,
  updateLeadEnquiry
);

router.get('/get-all', authMiddleware, getAllLeadEnquiry);

router.get('/get/:lead_enquiry_id', authMiddleware, getByLeadEnquiryId);

router.delete(
  '/delete/:lead_enquiry_id',
  authMiddleware,
  deleteByLeadEnquiryId
);

router.post('/search', authMiddleware, searchLeadEnquiry);

export default router;
