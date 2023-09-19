import { check } from 'express-validator';

export const vendorQuotesCreateValidator = [
    check('purchase_request_id')
        .not()
        .isEmpty()
        .withMessage('purchase_request_id is required'),
    check('vendor_id')
        .not()
        .isEmpty()
        .withMessage('vendor_id is required'),
];

export const vendorQuotesUpdateValidator = [
    check('vendor_quotes_id')
        .not()
        .isEmpty()
        .withMessage('vendor_id is required'),
    check('purchase_request_id')
        .not()
        .isEmpty()
        .withMessage('purchase_request_id is required'),
    check('vendor_id')
        .not()
        .isEmpty()
        .withMessage('vendor_id is required'),
];
