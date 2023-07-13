import { check } from 'express-validator';

export const uomCreateValidator = [
  check('name').not().isEmpty().withMessage('name is required'),
];

export const uomUpdateValidator = [
  check('uom_id').not().isEmpty().withMessage('uom_id is required'),
];
