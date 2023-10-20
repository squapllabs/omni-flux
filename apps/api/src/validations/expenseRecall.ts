import { check } from 'express-validator';

export const expenseRecallCreateValidator = [
  check('expense_id').not().isEmpty().withMessage('expense_id is required'),
  check('expense_details_id')
    .not()
    .isEmpty()
    .withMessage('expense_details_id is required'),
];
