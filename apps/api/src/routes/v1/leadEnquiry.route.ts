import express from 'express';
import authMiddleware from '../../middleware/auth';
import { leadEnquiryUpdateValidator } from '../../validations/leadEnquiry';
import { runValidation } from '../../validations/index';
import {
  checkDuplicateTenderIdentificationNo,
  checkDuplicateTenderRegNo,
  createLeadEnquiry,
  deleteByLeadEnquiryId,
  generateLeadCode,
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

router.get(
  '/check-duplicate-tender-reg-no/:tender_reg_no',
  authMiddleware,
  checkDuplicateTenderRegNo
);

router.get(
  '/check-duplicate-tender-identification-no/:tender_identification_no',
  authMiddleware,
  checkDuplicateTenderIdentificationNo
);

router.get('/generate-lead-code/:lead_type', authMiddleware, generateLeadCode);

export default router;
