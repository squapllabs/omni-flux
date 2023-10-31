import { check } from 'express-validator';

export const purchaseOrderUpdateValidator = [
  check('purchase_order_id')
    .not()
    .isEmpty()
    .withMessage('purchase_order_id is required'),
];
