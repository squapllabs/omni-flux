import { check } from 'express-validator';

export const inventoryCreateValidator = [
  check('item_id').not().isEmpty().withMessage('item_id is required'),
  check('available_quantity')
    .not()
    .isEmpty()
    .withMessage('available_quantity is required'),
  check('store_id').not().isEmpty().withMessage('store_id is required'),
];

export const inventoryUpdateValidator = [
  check('inventory_id').not().isEmpty().withMessage('inventory_id is required'),
];
