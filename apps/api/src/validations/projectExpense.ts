import { check } from 'express-validator';

export const projectExpenseUpdateValidator = [
  check('project_expense_id')
    .not()
    .isEmpty()
    .withMessage('project_expense_id is required'),
];
