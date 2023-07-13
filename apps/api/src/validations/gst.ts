import { check } from 'express-validator';

export const gstUpdateValidator = [
  check('gst_id').not().isEmpty().withMessage('gst_id is required'),
];
