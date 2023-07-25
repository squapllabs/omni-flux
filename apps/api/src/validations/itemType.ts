import { check } from 'express-validator';

export const itemTypeUpdateValidator = [
  check('item_type_id').not().isEmpty().withMessage('item_type_id is required'),
];