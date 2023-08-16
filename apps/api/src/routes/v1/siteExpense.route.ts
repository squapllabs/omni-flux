import express from 'express';
import authMiddleware from '../../middleware/auth';
import { siteExpenseUpdateValidator } from '../../validations/siteExpense';
import {
  createSiteExpense,
  deleteBySiteExpenseId,
  getAllSiteExpense,
  getByProjectIdAndSiteId,
  getBySiteExpenseId,
  searchSiteExpense,
  updateSiteExpense,
} from '../../controller/siteExpense.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/', authMiddleware, runValidation, createSiteExpense);

router.put(
  '/',
  authMiddleware,
  siteExpenseUpdateValidator,
  runValidation,
  updateSiteExpense
);

router.get('/getAll', authMiddleware, getAllSiteExpense);

router.get('/get/:site_expense_id', authMiddleware, getBySiteExpenseId);

router.delete(
  '/delete/:site_expense_id',
  authMiddleware,
  deleteBySiteExpenseId
);

router.post('/search', authMiddleware, searchSiteExpense);

router.get(
  '/get-by-project-id-and-site-id/:project_id/:site_id',
  authMiddleware,
  getByProjectIdAndSiteId
);

export default router;
