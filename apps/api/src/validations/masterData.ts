import { check } from 'express-validator';

export const masterDataCreateValidator = [
  check('master_data_name')
    .not()
    .isEmpty()
    .withMessage('master_data_name is required'),
  check('master_data_type')
    .not()
    .isEmpty()
    .withMessage('master_data_type is required'),
];

export const masterDataUpdateValidator = [
  check('master_data_id')
    .not()
    .isEmpty()
    .withMessage('master_data_id is required'),
];
