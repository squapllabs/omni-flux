import { check } from 'express-validator';

export const warehouseCreateValidator = [
  check('warehouse_name')
    .not()
    .isEmpty()
    .withMessage('warehouse_name is required'),
];

export const warehouseUpdateValidator = [
  check('warehouse_id').not().isEmpty().withMessage('warehouse_id is required'),
];
