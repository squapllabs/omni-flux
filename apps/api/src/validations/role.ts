import { check } from 'express-validator';

export const roleCreateValidator = [
  check('role_name').not().isEmpty().withMessage('role_name is required'),
];

export const roleUpdateValidator = [
  check('role_name').not().isEmpty().withMessage('role_name is required'),
  check('role_id').not().isEmpty().withMessage('role_id is required'),
];
