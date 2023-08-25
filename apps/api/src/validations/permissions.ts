import { check } from 'express-validator';

export const permissionsCreateValidator = [
  check('role_id').not().isEmpty().withMessage('role_id is required'),
  check('capability_id')
    .not()
    .isEmpty()
    .withMessage('capability_id is required'),
];

export const permissionsUpdateValidator = [
  check('permission_id')
    .not()
    .isEmpty()
    .withMessage('permission_id is required'),
];
