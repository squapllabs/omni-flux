import { check } from 'express-validator';

export const siteCreateValidator = [
  check('site_name').not().isEmpty().withMessage('site_name is required'),
];

export const siteUpdateValidator = [
  check('site_id').not().isEmpty().withMessage('site_id is required'),
];
