import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  siteContractorCreateValidator,
  siteContractorUpdateValidator,
} from '../../validations/siteContractor';
import { runValidation } from '../../validations/index';
import {
  createSiteContractor,
  deleteSiteContractorById,
  getAllContractors,
  getAllSiteContractors,
  getAllSites,
  getBySiteContractorId,
  updateSiteContractor,
} from '../../controller/siteContractor.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  siteContractorCreateValidator,
  runValidation,
  createSiteContractor
);

router.put(
  '/',
  authMiddleware,
  siteContractorUpdateValidator,
  runValidation,
  updateSiteContractor
);

router.get('/get-all', authMiddleware, getAllSiteContractors);

router.get('/get/:site_contractor_id', authMiddleware, getBySiteContractorId);

router.delete(
  '/delete/:site_contractor_id',
  authMiddleware,
  deleteSiteContractorById
);

router.get('/get-all-sites', authMiddleware, getAllSites);

router.get('/get-all-contractors', authMiddleware, getAllContractors);

export default router;
