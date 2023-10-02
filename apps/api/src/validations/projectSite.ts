import { check } from 'express-validator';

export const projectSiteCreateValidator = [
  check('project_id').not().isEmpty().withMessage('project_id is required'),
  check('site_id').not().isEmpty().withMessage('site_id is required'),
];

export const projectSiteUpdateValidator = [
  check('project_site_id')
    .not()
    .isEmpty()
    .withMessage('project_site_id is required'),
];
