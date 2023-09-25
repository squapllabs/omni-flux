import express from 'express';
import authMiddleware from '../../middleware/auth';
import { expenseDetailsUpdateValidator } from '../../validations/expenseDetails';
import { updateStatus } from '../../controller/expenseDetails.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.put(
  '/update-status',
  authMiddleware,
  expenseDetailsUpdateValidator,
  runValidation,
  updateStatus
);

export default router;
