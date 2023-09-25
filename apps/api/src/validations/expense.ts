import { check } from 'express-validator';

export const expenseUpdateValidator = [
  check('expense_id').not().isEmpty().withMessage('expense_id is required'),
];
