import { check } from 'express-validator';

export const expenseDetailsUpdateValidator = [
  check('expense_details_id')
    .not()
    .isEmpty()
    .withMessage('expense_details_id is required'),
];
