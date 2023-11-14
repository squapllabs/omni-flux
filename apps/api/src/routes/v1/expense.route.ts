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
  updateStatus,
  getByExpenseCode,
  addIndependentExpense,
  updateIndependentExpense,
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
  authMiddleware,
  getExpenseDetailsByExpenseId
);

router.put(
  '/update-status',
  expenseUpdateValidator,
  runValidation,
  authMiddleware,
  updateStatus
);

router.get(
  '/get-by-expense-code/:expense_code',
  authMiddleware,
  getByExpenseCode
);

router.post(
  '/addIndependentExpense',
  authMiddleware,
  runValidation,
  addIndependentExpense
);

router.put(
  '/updateIndependentExpense',
  authMiddleware,
  expenseUpdateValidator,
  runValidation,
  updateIndependentExpense
);

export default router;
