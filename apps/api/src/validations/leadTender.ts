import { check } from 'express-validator';

export const leadTenderUpdateValidator = [
  check('lead_tender_id')
    .not()
    .isEmpty()
    .withMessage('lead_tender_id is required'),
];
