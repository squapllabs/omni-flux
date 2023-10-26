import { check } from 'express-validator';

export const vendorQuotationDetailsUpdateValidator = [
  check('vendor_quotation_details_id')
    .not()
    .isEmpty()
    .withMessage('vendor_quotation_details_id is required'),
];
