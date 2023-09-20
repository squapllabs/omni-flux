import { check } from 'express-validator';

export const stockOutwardCreateValidator = [
  check('project_id').not().isEmpty().withMessage('project_id is required'),
  check('site_engineer_id')
    .not()
    .isEmpty()
    .withMessage('site_engineer_id is required'),
  check('site_id').not().isEmpty().withMessage('site_id is required'),
  check('stock_outward_date')
    .not()
    .isEmpty()
    .withMessage('stock_outward_date is required'),
];

export const stockOutwardUpdateValidator = [
  check('stock_outward_id')
    .not()
    .isEmpty()
    .withMessage('stock_outward_id is required'),
];
