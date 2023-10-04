import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  addItem,
  updateItem,
  deleteByItemId,
  getAllItem,
  getByItemId,
  addBulkItems,
  getAllItemBySearch,
  getAllItemData,
  search,
  checkDuplicateName,
  getByIndentRequestId,
} from '../../controller/item.controller';
import { itemUpdateValidator } from '../../validations/item';
import { runValidation } from '../../validations/index';
import multer from 'multer';
const router = express.Router();
const upload = multer();

router.post('/add-item', authMiddleware, addItem);
router.post(
  '/add-bulk-items',
  authMiddleware,
  upload.single('excelFile'),
  addBulkItems
);
router.post('/get-all-items', authMiddleware, getAllItem);
router.get('/get/:item_id', authMiddleware, getByItemId);
router.delete('/delete/:item_id', authMiddleware, deleteByItemId);
router.put(
  '/update-item',
  authMiddleware,
  itemUpdateValidator,
  runValidation,
  updateItem
);
router.post('/get-all-items-by-search', authMiddleware, getAllItemBySearch);

router.get('/get-all', authMiddleware, getAllItemData);

router.post('/search', authMiddleware, search);

router.get(
  '/check-duplicate-name/:item_name',
  authMiddleware,
  checkDuplicateName
);

router.get(
  '/get-by-indent-request-id/:indent_request_id',
  authMiddleware,
  getByIndentRequestId
);

export default router;
