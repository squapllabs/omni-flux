import { check } from 'express-validator';

export const bomConfigurationUpdateValidator = [
  check('bom_configuration_id')
    .not()
    .isEmpty()
    .withMessage('bom_configuration_id is required'),
];
