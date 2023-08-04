import { check } from 'express-validator';

export const siteContractorCreateValidator = [
  check('name').not().isEmpty().withMessage('name is required'),
  check('type').not().isEmpty().withMessage('type is required'),
];

export const siteContractorUpdateValidator = [
  check('site_contractor_id')
    .not()
    .isEmpty()
    .withMessage('site_contractor_id is required'),
];
