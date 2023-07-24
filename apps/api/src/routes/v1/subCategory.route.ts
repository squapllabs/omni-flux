import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  subCategoryCreateValidator,
  subCategoryUpdateValidator,
} from '../../validations/subCategory';
import {
  checkDuplicateSubCategoryName,
  createSubCategory,
  deleteBySubCategoryId,
  getAllInActiveSubCategories,
  getAllSubCategory,
  getBySubCategoryId,
  updateSubCategory,
} from '../../controller/subCategory.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  subCategoryCreateValidator,
  runValidation,
  createSubCategory
);

router.put(
  '/',
  authMiddleware,
  subCategoryUpdateValidator,
  runValidation,
  updateSubCategory
);

router.get('/getAll', authMiddleware, getAllSubCategory);

router.get('/get/:sub_category_id', authMiddleware, getBySubCategoryId);

router.delete(
  '/delete/:sub_category_id',
  authMiddleware,
  deleteBySubCategoryId
);

router.get(
  '/checkDuplicateName/:sub_category_name/:category_id',
  authMiddleware,
  checkDuplicateSubCategoryName
);

router.get('/get-all-inactive', authMiddleware, getAllInActiveSubCategories);

export default router;
