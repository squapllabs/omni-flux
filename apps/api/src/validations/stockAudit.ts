import { check } from 'express-validator';

export const stockAuditCreateValidator = [
  check('project_id').not().isEmpty().withMessage('project_id is required'),
];

export const stockAuditUpdateValidator = [
  check('stock_audit_id')
    .not()
    .isEmpty()
    .withMessage('stock_audit_id is required'),
];
