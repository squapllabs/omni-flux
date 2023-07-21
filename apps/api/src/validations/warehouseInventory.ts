import { check } from 'express-validator';

export const warehouseInventoryUpdateValidator = [
  check('warehouse_inventory_id')
    .not()
    .isEmpty()
    .withMessage('warehouse_inventory_id is required'),
];
