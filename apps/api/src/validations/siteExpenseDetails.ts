import { check } from 'express-validator';

export const siteExpenseDetailsCreateValidator = [
  check('project_id').not().isEmpty().withMessage('project_id is required'),
  check('site_id').not().isEmpty().withMessage('site_id is required'),
];
