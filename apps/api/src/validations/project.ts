import { check } from 'express-validator';

export const projectCreateValidator = [
  check('project_name').not().isEmpty().withMessage('project_name is required'),
];

export const projectUpdateValidator = [
  check('project_id').not().isEmpty().withMessage('project_id is required'),
];
