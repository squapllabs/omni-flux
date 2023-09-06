import { check } from 'express-validator';

export const machineryCreateValidator = [
    check('machinery_name').not().isEmpty().withMessage('machinery_name is required'),
];

export const machineryUpdateValidator = [
    check('machinery_id')
        .not()
        .isEmpty()
        .withMessage('machinery_id is required'),
];
