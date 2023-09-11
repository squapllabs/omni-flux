import { check } from 'express-validator';

export const projectMemberAssociationCreateValidator = [
  check('project_id').not().isEmpty().withMessage('project_id is required'),
  check('user_id').not().isEmpty().withMessage('user_id is required'),
];

export const projectMemberAssociationUpdateValidator = [
  check('project_member_association_id')
    .not()
    .isEmpty()
    .withMessage('project_member_association_id is required'),
  check('project_id').not().isEmpty().withMessage('project_id is required'),
  check('user_id').not().isEmpty().withMessage('user_id is required'),
];
