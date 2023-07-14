import { check } from 'express-validator';

export const subCategoryCreateValidator = [
  check('name').not().isEmpty().withMessage('name is required'),
];

export const subCategoryUpdateValidator = [
  check('sub_category_id')
    .not()
    .isEmpty()
    .withMessage('sub_category_id is required'),
];
