import express from 'express';
import authMiddleware from '../../middleware/auth';
import { siteExpenseDetailsCreateValidator } from '../../validations/siteExpenseDetails';
import { runValidation } from '../../validations/index';
import {
  addBulkSiteExpenseDetails,
  createSiteExpenseDetails,
} from '../../controller/siteExpenseDetails.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  siteExpenseDetailsCreateValidator,
  runValidation,
  createSiteExpenseDetails
);

router.post(
  '/add-bulk',
  authMiddleware,
  siteExpenseDetailsCreateValidator,
  runValidation,
  addBulkSiteExpenseDetails
);

export default router;
