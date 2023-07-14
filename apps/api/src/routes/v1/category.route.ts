import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  categoryCreateValidator,
  categoryUpdateValidator,
} from '../../validations/category';
import {
  createCategory,
  deleteByCategoryId,
  getAllCategory,
  getByCategoryId,
  updateCategory,
} from '../../controller/category.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  categoryCreateValidator,
  runValidation,
  createCategory
);

router.put(
  '/',
  authMiddleware,
  categoryUpdateValidator,
  runValidation,
  updateCategory
);

router.get('/getAll', authMiddleware, getAllCategory);

router.get('/get/:category_id', authMiddleware, getByCategoryId);

router.delete('/delete/:category_id', authMiddleware, deleteByCategoryId);

export default router;
