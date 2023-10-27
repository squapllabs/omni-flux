import express from 'express';
import authMiddleware from '../../middleware/auth';
import { vendorQuotationDetailsUpdateValidator } from '../../validations/vendorQuotationDetails';
import {
  getByVendorQuotesId,
  updateVendorQuotationDetails,
} from '../../controller/vendorQuotationDetails.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.put(
  '/',
  authMiddleware,
  vendorQuotationDetailsUpdateValidator,
  runValidation,
  updateVendorQuotationDetails
);

router.get(
  '/get-by-vendor-quotes-id/:vendor_quotes_id',
  authMiddleware,
  getByVendorQuotesId
);

export default router;
