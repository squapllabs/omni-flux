import { check } from 'express-validator';

export const purchaseOrderCreateValidator = [
  check('purchase_request_id')
    .not()
    .isEmpty()
    .withMessage('purchase_request_id is required'),
];

export const purchaseOrderUpdateValidator = [
  check('purchase_order_id')
    .not()
    .isEmpty()
    .withMessage('purchase_order_id is required'),
];
