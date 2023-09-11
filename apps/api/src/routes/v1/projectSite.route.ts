import express from 'express';
import authMiddleware from '../../middleware/auth';
import { getByProjectId } from '../../controller/projectSite.controller';

const router = express.Router();

router.get(
  '/get-sites-by-project-id/:project_id',
  authMiddleware,
  getByProjectId
);

export default router;
