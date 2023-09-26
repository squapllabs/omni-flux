import express from 'express';
import authMiddleware from '../../middleware/auth';
import { expenseDetailsUpdateValidator } from '../../validations/expenseDetails';
import {
  getById,
  searchExpenseDetails,
  updateStatus,
} from '../../controller/expenseDetails.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.put(
  '/update-status',
  authMiddleware,
  expenseDetailsUpdateValidator,
  runValidation,
  updateStatus
);

router.get('/get/:expense_details_id', authMiddleware, getById);

router.post('/search', authMiddleware, searchExpenseDetails);

export default router;
