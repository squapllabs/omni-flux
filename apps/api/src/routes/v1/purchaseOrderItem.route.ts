import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  purchaseOrderItemCreateValidator,
  purchaseOrderItemUpdateValidator,
} from '../../validations/purchaseOrderItem';
import {
  createPurchaseOrderItem,
  deleteByPurchaseOrderItemId,
  getAllPurchaseOrderItems,
  getByPurchaseOrderItemId,
  searchPurchaseOrderItem,
  updatePurchaseOrderItem,
} from '../../controller/purchaseOrderItem.controller';
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
  purchaseOrderItemCreateValidator,
  runValidation,
  createPurchaseOrderItem
);

router.put(
  '/',
  authMiddleware,
  upload.fields([
    {
      name: 'purchase_order_item_documents',
      maxCount: Number(process.env.MAX_COUNT_FOR_PURCHASE_REQUEST_DOC),
    },
  ]),
  purchaseOrderItemUpdateValidator,
  runValidation,
  updatePurchaseOrderItem
);

router.get('/get-all', authMiddleware, getAllPurchaseOrderItems);

router.get(
  '/get/:purchase_order_item_id',
  authMiddleware,
  getByPurchaseOrderItemId
);

router.delete(
  '/delete/:purchase_order_item_id',
  authMiddleware,
  deleteByPurchaseOrderItemId
);

router.post('/search', authMiddleware, searchPurchaseOrderItem);

export default router;
