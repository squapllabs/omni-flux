import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  purchaseRequestCreateValidator,
  purchaseRequestUpdateValidator,
} from '../../validations/purchaseRequest';
import {
  createPurchaseRequest,
  deleteByPurchaseRequestId,
  getAllPurchaseRequestProjectsByStatus,
  getAllPurchaseRequests,
  getByPurchaseRequestId,
  searchPurchaseRequest,
  updatePurchaseRequest,
} from '../../controller/purchaseRequest.controller';
import { runValidation } from '../../validations/index';
import multer from 'multer';
const storage = multer.diskStorage({
  destination: function (req, file, cb) {
    const uploadPath = 'tmp/';
    cb(null, uploadPath);
  },
  filename: function (req, file, cb) {
    const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1e9);
    cb(null, file.fieldname + '-' + uniqueSuffix + '-' + file.originalname);
  },
});
const upload = multer({ storage });
const router = express.Router();

router.post(
  '/',
  authMiddleware,
  purchaseRequestCreateValidator,
  runValidation,
  createPurchaseRequest
);

router.put(
  '/',
  authMiddleware,
  upload.fields([
    {
      name: 'purchase_request_documents',
      maxCount: Number(process.env.MAX_COUNT_FOR_PURCHASE_REQUEST_DOC),
    },
  ]),
  purchaseRequestUpdateValidator,
  runValidation,
  updatePurchaseRequest
);

router.get('/get-all', authMiddleware, getAllPurchaseRequests);

router.get('/get/:purchase_request_id', authMiddleware, getByPurchaseRequestId);

router.delete(
  '/delete/:purchase_request_id',
  authMiddleware,
  deleteByPurchaseRequestId
);

router.post('/search', authMiddleware, searchPurchaseRequest);

router.get(
  '/get-all-projects-by-status/:status',
  authMiddleware,
  getAllPurchaseRequestProjectsByStatus
);

export default router;
