import { check } from 'express-validator';

export const bomConfigurationCreateValidator = [
  check('bom_name').not().isEmpty().withMessage('bom_name is required'),
];
export const bomConfigurationUpdateValidator = [
  check('bom_configuration_id')
    .not()
    .isEmpty()
    .withMessage('bom_configuration_id is required'),
];
