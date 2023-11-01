import { check } from 'express-validator';

export const grnCreateValidator = [
  check('project_id').not().isEmpty().withMessage('project_id is required'),
  check('purchase_order_id')
    .not()
    .isEmpty()
    .withMessage('purchase_order_id is required'),
  check('goods_received_by')
    .not()
    .isEmpty()
    .withMessage('goods_received_by is required'),
];
