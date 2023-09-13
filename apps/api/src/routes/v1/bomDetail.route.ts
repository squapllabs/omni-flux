import express from 'express';
import {
  bomCreateValidator,
  bomUpdateValidator,
} from '../../validations/bomDetail';
import {
  createBom,
  deleteBom,
  getByBomId,
  updateBom,
  getAllBom,
  getByCategorySubCatAndSubSubCatId,
  fetchEntireDataByBomId,
  addBulkBom,
  getBomBySubCategoryIdAndBomType,
  getBomTotalBySubCategoryId,
  getBySubCategoryId,
  getByProjectIdAndBomType,
  fetchEntireTotalDataBySubCategoryId,
} from '../../controller/bomDetail.controller';
import authMiddleware from '../../middleware/auth';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/', authMiddleware, bomCreateValidator, runValidation, createBom);
router.put('/', authMiddleware, bomUpdateValidator, runValidation, updateBom);

router.get('/get-all', authMiddleware, getAllBom);

router.get('/get/:bom_id', authMiddleware, getByBomId);

router.delete('/delete/:bom_id', authMiddleware, deleteBom);

router.post(
  '/get-by-category-combo',
  authMiddleware,
  getByCategorySubCatAndSubSubCatId
);

router.get('/get-entire-data/:bom_id', authMiddleware, fetchEntireDataByBomId);

router.post('/add-bulk', authMiddleware, addBulkBom);

router.get(
  '/get-by-sub-cat-id-and-bom-type/:sub_category_id/:bom_type',
  authMiddleware,
  getBomBySubCategoryIdAndBomType
);

router.get(
  '/get-bom-total-by-sub-cat-id/:sub_category_id',
  authMiddleware,
  getBomTotalBySubCategoryId
);

router.get(
  '/get-by-sub-category-id/:sub_category_id',
  authMiddleware,
  getBySubCategoryId
);

router.get(
  '/get-by-project-id-and-bom-type/:project_id/:bom_type',
  authMiddleware,
  getByProjectIdAndBomType
);

router.get('/get-sum-of-total-by-sub-category-id/:sub_category_id',
  authMiddleware,
  fetchEntireTotalDataBySubCategoryId
);

export default router;
