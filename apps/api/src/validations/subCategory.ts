import { check } from 'express-validator';

export const subCategoryUpdateValidator = [
  check('sub_category_id')
    .not()
    .isEmpty()
    .withMessage('sub_category_id is required'),
];
