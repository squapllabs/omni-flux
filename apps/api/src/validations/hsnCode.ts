import { check } from 'express-validator';

export const hsnCodeCreateValidator = [
  check('code').not().isEmpty().withMessage('code is required'),
];

export const hsnCodeUpdateValidator = [
  check('hsn_code_id').not().isEmpty().withMessage('hsn_code_id is required'),
];
