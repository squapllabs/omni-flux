import express from 'express';
import authMiddleware from '../../middleware/auth';
import { projectExpenseUpdateValidator } from '../../validations/projectExpense';
import {
  createProjectExpense,
  deleteByPojectExpenseId,
  getAllProjectExpense,
  getByProjectExpenseId,
  updateProjectExpense,
} from '../../controller/projectExpense.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/', authMiddleware, runValidation, createProjectExpense);

router.put(
  '/',
  authMiddleware,
  projectExpenseUpdateValidator,
  runValidation,
  updateProjectExpense
);

router.get('/getAll', authMiddleware, getAllProjectExpense);

router.get('/get/:project_expense_id', authMiddleware, getByProjectExpenseId);

router.delete(
  '/delete/:project_expense_id',
  authMiddleware,
  deleteByPojectExpenseId
);

export default router;
