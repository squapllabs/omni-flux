import { check } from 'express-validator';

export const leadProductCreateValidator = [
  check('source_name').not().isEmpty().withMessage('source_name is required'),
];

export const leadProductUpdateValidator = [
  check('lead_product_id')
    .not()
    .isEmpty()
    .withMessage('lead_product_id is required'),
];
