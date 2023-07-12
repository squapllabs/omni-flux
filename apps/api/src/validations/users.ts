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
  check('first_name').not().isEmpty().withMessage('first_name is required'),
  check('last_name').not().isEmpty().withMessage('last_name is required'),
];

export const userUpdateValidator = [
  check('user_id').not().isEmpty().withMessage('user_id is required'),
  check('email_id').not().isEmpty().withMessage('email_id is required'),
  check('first_name').not().isEmpty().withMessage('first_name is required'),
  check('last_name').not().isEmpty().withMessage('last_name is required'),
];
