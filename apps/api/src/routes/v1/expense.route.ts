import express from 'express';
import authMiddleware from '../../middleware/auth';
import { expenseUpdateValidator } from '../../validations/expense';
import {
  createExpense,
  deleteByExpenseId,
  getAllExpense,
  getByProjectIdAndSiteId,
  getByExpenseId,
  searchExpense,
  updateExpense,
  getExpenseDetailsByExpenseId,
} from '../../controller/expense.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/', authMiddleware, runValidation, createExpense);

router.put(
  '/',
  authMiddleware,
  expenseUpdateValidator,
  runValidation,
  updateExpense
);

router.get('/get-all', authMiddleware, getAllExpense);

router.get('/get/:expense_id', authMiddleware, getByExpenseId);

router.delete('/delete/:expense_id', authMiddleware, deleteByExpenseId);

router.post('/search', authMiddleware, searchExpense);

router.get(
  '/get-by-project-id-and-site-id/:project_id/:site_id',
  authMiddleware,
  getByProjectIdAndSiteId
);

router.get(
  '/get-expense-details-by-expense-id/:expense_id',
  getExpenseDetailsByExpenseId
);

export default router;
