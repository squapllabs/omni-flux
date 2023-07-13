import { check } from 'express-validator';

export const clientCreateValidator = [
  check('name').not().isEmpty().withMessage('name is required'),
  check('contact_details')
    .not()
    .isEmpty()
    .withMessage('contact_details is required'),
];

export const clientUpdateValidator = [
  check('client_id').not().isEmpty().withMessage('client_id is required'),
];
