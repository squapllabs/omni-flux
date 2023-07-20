import { check } from 'express-validator';

export const siteExpenseUpdateValidator = [
  check('site_expense_id')
    .not()
    .isEmpty()
    .withMessage('site_expense_id is required'),
];
