import express from 'express';
import authMiddleware from '../../middleware/auth';
import { expenseRecallCreateValidator } from '../../validations/expenseRecall';
import { createExpenseRecall } from '../../controller/expenseRecall.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  expenseRecallCreateValidator,
  runValidation,
  createExpenseRecall
);

export default router;
