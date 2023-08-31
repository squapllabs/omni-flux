import { check } from 'express-validator';

export const vendorCreateValidator = [
  check('vendor_name').not().isEmpty().withMessage('vendor_name is required'),
];

export const vendorUpdateValidator = [
  check('vendor_id').not().isEmpty().withMessage('vendor_id is required'),
];
