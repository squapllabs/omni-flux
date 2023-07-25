import { check } from 'express-validator';

export const brandUpdateValidator = [
  check('brand_id').not().isEmpty().withMessage('brand_id is required'),
];