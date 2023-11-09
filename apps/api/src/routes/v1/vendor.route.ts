import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  vendorCreateValidator,
  vendorUpdateValidator,
} from '../../validations/vendor';
import { runValidation } from '../../validations/index';
import {
  createVendor,
  deleteByVendorId,
  getAllVendor,
  getByEmailId,
  getByVendorId,
  getByVendorName,
  searchVendor,
  updateVendor,
} from '../../controller/vendor.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  vendorCreateValidator,
  runValidation,
  createVendor
);

router.put(
  '/',
  authMiddleware,
  vendorUpdateValidator,
  runValidation,
  updateVendor
);

router.get('/get-all', authMiddleware, getAllVendor);

router.get('/get/:vendor_id', authMiddleware, getByVendorId);

router.delete('/delete/:vendor_id', authMiddleware, deleteByVendorId);

router.post('/search', authMiddleware, searchVendor);

router.get('/get-by-email-id/:contact_email', authMiddleware, getByEmailId);

router.get(
  '/check-duplicate-name/:vendor_name',
  authMiddleware,
  getByVendorName
);

export default router;
