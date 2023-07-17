import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  subSubCategoryCreateValidator,
  subSubCategoryUpdateValidator,
} from '../../validations/subSubCategory';
import { runValidation } from '../../validations/index';
import {
  createSubSubCategory,
  deleteBySubSubCategoryId,
  getAllSubSubCategory,
  getBySubSubCategoryId,
  updateSubSubCategory,
} from '../../controller/subSubCategory.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  subSubCategoryCreateValidator,
  runValidation,
  createSubSubCategory
);

router.put(
  '/',
  authMiddleware,
  subSubCategoryUpdateValidator,
  runValidation,
  updateSubSubCategory
);

router.get('/getAll', authMiddleware, getAllSubSubCategory);

router.get('/get/:sub_sub_category_id', authMiddleware, getBySubSubCategoryId);

router.delete(
  '/delete/:sub_sub_category_id',
  authMiddleware,
  deleteBySubSubCategoryId
);

export default router;
