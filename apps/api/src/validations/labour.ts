import { check } from 'express-validator';

export const labourCreateValidator = [
  check('labour_type').not().isEmpty().withMessage('labour_type is required'),
];

export const labourUpdateValidator = [
  check('labour_id').not().isEmpty().withMessage('labour_id is required'),
];
