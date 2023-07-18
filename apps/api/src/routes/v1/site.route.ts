import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  siteCreateValidator,
  siteUpdateValidator,
} from '../../validations/site';
import { runValidation } from '../../validations/index';
import {
  createSite,
  deleteBySiteId,
  getAllSites,
  getBySiteId,
  updateSite,
} from '../../controller/site.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  siteCreateValidator,
  runValidation,
  createSite
);

router.put('/', authMiddleware, siteUpdateValidator, runValidation, updateSite);

router.get('/getAll', authMiddleware, getAllSites);

router.get('/get/:site_id', authMiddleware, getBySiteId);

router.delete('/delete/:site_id', authMiddleware, deleteBySiteId);

export default router;
