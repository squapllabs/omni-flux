import { check } from 'express-validator';

export const purchaseOrderItemCreateValidator = [
  check('purchase_order_id')
    .not()
    .isEmpty()
    .withMessage('purchase_order_id is required'),
];

export const purchaseOrderItemUpdateValidator = [
  check('purchase_order_id')
    .not()
    .isEmpty()
    .withMessage('purchase_order_id is required'),
  check('purchase_order_item_id')
    .not()
    .isEmpty()
    .withMessage('purchase_order_item_id is required'),
];
