import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  projectCreateValidator,
  projectUpdateValidator,
} from '../../validations/project';
import {
  checkDuplicateCode,
  createProject,
  deleteByProjectId,
  getAllProject,
  getByProjectId,
  getByProjectIdAndSiteId,
  searchProject,
  updateProject,
} from '../../controller/project.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  projectCreateValidator,
  runValidation,
  createProject
);

router.put(
  '/',
  authMiddleware,
  projectUpdateValidator,
  runValidation,
  updateProject
);

router.get('/getAll', authMiddleware, getAllProject);

router.get('/get/:project_id', authMiddleware, getByProjectId);

router.delete('/delete/:project_id', authMiddleware, deleteByProjectId);

router.post('/search', authMiddleware, searchProject);

router.get('/check-duplicate-code/:code', authMiddleware, checkDuplicateCode);

router.post('/get-project-site', authMiddleware, getByProjectIdAndSiteId);

export default router;
