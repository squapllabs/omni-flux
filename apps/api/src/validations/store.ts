import { check } from 'express-validator';

export const storeCreateValidator = [
  check('store_name').not().isEmpty().withMessage('store_name is required'),
  check('store_manager_id')
    .not()
    .isEmpty()
    .withMessage('store_manager_id is required'),
];

export const storeUpdateValidator = [
  check('store_id').not().isEmpty().withMessage('store_id is required'),
];
