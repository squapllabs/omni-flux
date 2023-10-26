import express from 'express';
import authMiddleware from '../../middleware/auth';
import { vendorQuotationDetailsUpdateValidator } from '../../validations/vendorQuotationDetails';
import { updateVendorQuotationDetails } from '../../controller/vendorQuotationDetails.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.put(
  '/',
  authMiddleware,
  vendorQuotationDetailsUpdateValidator,
  runValidation,
  updateVendorQuotationDetails
);

export default router;
