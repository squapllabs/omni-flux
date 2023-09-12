import { check } from 'express-validator';

export const userPrimaryProjectCreateValidator = [
  check('user_id').not().isEmpty().withMessage('user_id is required'),
  check('project_id').not().isEmpty().withMessage('project_id is required'),
];

export const userPrimaryProjectUpdateValidator = [
  check('user_primary_project_id')
    .not()
    .isEmpty()
    .withMessage('user_primary_project_id is required'),
  check('user_id').not().isEmpty().withMessage('user_id is required'),
  check('project_id').not().isEmpty().withMessage('project_id is required'),
];
