import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  categoryCreateValidator,
  categoryUpdateValidator,
} from '../../validations/category';
import {
  checkDuplicateProjectCategoryName,
  createCategory,
  deleteByCategoryId,
  getAllCategory,
  getAllInActiveCategories,
  getByCategoryId,
  getByProjectId,
  searchCategory,
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

router.get(
  '/checkDuplicateName/:category_name/:project_id',
  authMiddleware,
  checkDuplicateProjectCategoryName
);

router.get('/get-all-inactive', authMiddleware, getAllInActiveCategories);

router.post('/search', authMiddleware, searchCategory);

router.get('/get-by-project-id/:project_id', authMiddleware, getByProjectId);

export default router;
