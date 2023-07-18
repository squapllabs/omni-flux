import { check } from 'express-validator';

export const productUpdateValidator = [
  check('product_id').not().isEmpty().withMessage('product_id is required'),
];