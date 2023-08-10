import { check } from 'express-validator';

export const processFileDeleteValidator = [
  check('path').not().isEmpty().withMessage('path is required'),
];
