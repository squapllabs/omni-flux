import { check } from 'express-validator';

export const itemUpdateValidator = [
  check('item_id').not().isEmpty().withMessage('item_id is required'),
];