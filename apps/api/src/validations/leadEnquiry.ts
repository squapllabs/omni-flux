import { check } from 'express-validator';

export const leadEnquiryUpdateValidator = [
  check('lead_enquiry_id')
    .not()
    .isEmpty()
    .withMessage('lead_enquiry_id is required'),
];
