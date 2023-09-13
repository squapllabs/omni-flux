import { check } from 'express-validator';

export const bomCreateValidator = [
    check('bom_name').not().isEmpty().withMessage('bom_name is required'),
];
export const bomUpdateValidator = [
    check('bom_id').not().isEmpty().withMessage('bom_id is required'),
];
