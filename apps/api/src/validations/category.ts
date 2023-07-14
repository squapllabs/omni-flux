import { check } from 'express-validator';

export const categoryCreateValidator = [
  check('name').not().isEmpty().withMessage('name is required'),
];

export const categoryUpdateValidator = [
  check('category_id').not().isEmpty().withMessage('category_id is required'),
];
