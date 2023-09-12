import { check } from 'express-validator';

export const indentRequestCreateValidator = [
  check('requester_user_id')
    .not()
    .isEmpty()
    .withMessage('requester_user_id is required'),
  check('requested_date')
    .not()
    .isEmpty()
    .withMessage('requested_date is required'),
];

export const indentRequestUpdateValidator = [
  check('indent_request_id')
    .not()
    .isEmpty()
    .withMessage('indent_request_id is required'),
];
