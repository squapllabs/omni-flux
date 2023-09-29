import { check } from 'express-validator';

export const projectInventoryCreateValidator = [
  check('project_id').not().isEmpty().withMessage('project_id is required'),
  check('item_id').not().isEmpty().withMessage('item_id is required'),
];

export const projectInventoryUpdateValidator = [
  check('project_inventory_id')
    .not()
    .isEmpty()
    .withMessage('project_inventory_id is required'),
];
