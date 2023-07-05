import { check } from 'express-validator';

export const userLoginValidator = [
  check('email_id').not().isEmpty().withMessage('email_id is required'),
  check('user_password')
    .not()
    .isEmpty()
    .withMessage('user_password is required'),
];

export const userCreateValidator = [
  check('email_id').not().isEmpty().withMessage('email_id is required'),
];
