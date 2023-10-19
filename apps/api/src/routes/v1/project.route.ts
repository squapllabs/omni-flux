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
  getAllDashboard,
  getAllProject,
  getByProjectId,
  getByProjectIdAndSiteId,
  getByUserId,
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

router.post('/check-duplicate-code', authMiddleware, checkDuplicateCode);

router.post('/get-project-site', authMiddleware, getByProjectIdAndSiteId);

router.get('/get-dashboard', authMiddleware, getAllDashboard);

router.get('/get-by-user-id/:user_id?', authMiddleware, getByUserId);

export default router;
