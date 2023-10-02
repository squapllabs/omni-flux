import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  createProjectSite,
  getAllProjectSites,
  getByProjectId,
  getByProjectSiteId,
  searchProjectSite,
  updateProjectSite,
} from '../../controller/projectSite.controller';
import { runValidation } from '../../validations/index';
import {
  projectSiteCreateValidator,
  projectSiteUpdateValidator,
} from '../../validations/projectSite';

const router = express.Router();

router.get(
  '/get-sites-by-project-id/:project_id',
  authMiddleware,
  getByProjectId
);

router.post(
  '/',
  authMiddleware,
  projectSiteCreateValidator,
  runValidation,
  createProjectSite
);

router.put(
  '/',
  authMiddleware,
  projectSiteUpdateValidator,
  runValidation,
  updateProjectSite
);

router.get('/get-all', authMiddleware, getAllProjectSites);

router.get(
  '/get-by-project-site-id/:project_site_id',
  authMiddleware,
  getByProjectSiteId
);

router.post('/search', authMiddleware, searchProjectSite);

export default router;
