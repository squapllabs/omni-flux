import { check } from 'express-validator';

export const purchaseRequestCreateValidator = [
  check('indent_request_id')
    .not()
    .isEmpty()
    .withMessage('indent_request_id is required'),
  check('requester_user_id')
    .not()
    .isEmpty()
    .withMessage('requester_user_id is required'),
];

export const purchaseRequestUpdateValidator = [
  check('purchase_request_id')
    .not()
    .isEmpty()
    .withMessage('purchase_request_id is required'),
];
