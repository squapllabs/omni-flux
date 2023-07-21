import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  projectCreateValidator,
  projectUpdateValidator,
} from '../../validations/project';
import {
  createProject,
  customFilterProject,
  deleteByPojectId,
  getAllProject,
  getByProjectId,
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

router.delete('/delete/:project_id', authMiddleware, deleteByPojectId);

router.post('/customFilter', authMiddleware, customFilterProject);

export default router;
