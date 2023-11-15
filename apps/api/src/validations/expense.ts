import { check } from 'express-validator';

export const expenseCreateValidator = [
  check('user_id').not().isEmpty().withMessage('user_id is required'),
];

export const expenseUpdateValidator = [
  check('expense_id').not().isEmpty().withMessage('expense_id is required'),
];

export const expenseUpdateByUserIdValidator = [
  check('expense_id').not().isEmpty().withMessage('expense_id is required'),
  check('user_id').not().isEmpty().withMessage('user_id is required'),
];
