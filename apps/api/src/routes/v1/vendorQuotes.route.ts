import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  vendorQuotesCreateValidator,
  vendorQuotesUpdateValidator,
} from '../../validations/vendorQuotes';
import {
  createVendorQuotes,
  deleteByVendorQuotesId,
  getAllVendorQuotes,
  getByPurchaseRequestId,
  getByPurchaseRequestIdAndVendorId,
  getByVendorQuotesId,
  getVendorDetailsByPurchaseRequestId,
  searchVendorQuotes,
  updateStatusAndDocument,
  updateVendorQuotes,
} from '../../controller/vendorQuotes.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  vendorQuotesCreateValidator,
  runValidation,
  createVendorQuotes
);

router.put(
  '/',
  authMiddleware,
  vendorQuotesUpdateValidator,
  runValidation,
  updateVendorQuotes
);

router.get('/get-all', authMiddleware, getAllVendorQuotes);

router.get('/get/:vendor_quotes_id', authMiddleware, getByVendorQuotesId);

router.delete(
  '/delete/:vendor_quotes_id',
  authMiddleware,
  deleteByVendorQuotesId
);

router.post('/search', authMiddleware, searchVendorQuotes);

router.put(
  '/update-status-and-document',
  authMiddleware,
  updateStatusAndDocument
);

router.get(
  '/check-duplicate-vendor-by-purchase-request-id/:purchase_request_id/:vendor_id',
  authMiddleware,
  getByPurchaseRequestIdAndVendorId
);

router.get(
  '/get-by-purchase-request-id/:purchase_request_id',
  authMiddleware,
  getByPurchaseRequestId
);

router.get(
  '/get-vendor-details-by-purchase-request-id/:purchase_request_id',
  authMiddleware,
  getVendorDetailsByPurchaseRequestId
);

export default router;
