import { check } from 'express-validator';

export const purchaseOrderInvoiceCreateValidator = [
  check('purchase_order_id')
    .not()
    .isEmpty()
    .withMessage('purchase_order_id is required'),
  check('grn_id').not().isEmpty().withMessage('grn_id is required'),
  check('requested_by').not().isEmpty().withMessage('requested_by is required'),
];

export const purchaseOrderInvoiceUpdateValidator = [
  check('purchase_order_invoice_id')
    .not()
    .isEmpty()
    .withMessage('purchase_order_invoice_id is required'),
];
