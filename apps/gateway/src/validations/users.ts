import { check } from 'express-validator';

export const loginValidator = [
  check('email_id').not().isEmpty().withMessage('email_id is required'),
  check('user_password')
    .not()
    .isEmpty()
    .withMessage('user_password is required'),
];
