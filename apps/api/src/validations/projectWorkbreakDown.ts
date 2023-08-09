import { check } from 'express-validator';

export const projectWorkbreakDownCreateValidator = [
  check('project_workbreak_down_name')
    .not()
    .isEmpty()
    .withMessage('project_workbreak_down_name is required'),
  check('project_workbreak_down_type')
    .not()
    .isEmpty()
    .withMessage('project_workbreak_down_type is required'),
];

export const projectWorkbreakDownUpdateValidator = [
  check('project_workbreak_down_id')
    .not()
    .isEmpty()
    .withMessage('project_workbreak_down_id is required'),
];
