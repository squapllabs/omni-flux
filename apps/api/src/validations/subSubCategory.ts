import { check } from 'express-validator';

export const subSubCategoryCreateValidator = [
  check('name').not().isEmpty().withMessage('name is required'),
];

export const subSubCategoryUpdateValidator = [
  check('sub_sub_category_id')
    .not()
    .isEmpty()
    .withMessage('sub_sub_category_id is required'),
];
