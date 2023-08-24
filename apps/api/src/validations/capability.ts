import { check } from 'express-validator';

export const capabilityCreateValidator = [
  check('capability_name')
    .not()
    .isEmpty()
    .withMessage('capability_name is required'),
];

export const capabilityUpdateValidator = [
  check('capability_id')
    .not()
    .isEmpty()
    .withMessage('capability_id is required'),
];
