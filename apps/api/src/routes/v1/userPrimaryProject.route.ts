import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  userPrimaryProjectCreateValidator,
  userPrimaryProjectUpdateValidator,
} from '../../validations/userPrimaryProject';
import { runValidation } from '../../validations/index';
import {
  createUserPrimaryProject,
  deleteByUserPrimaryProjectId,
  getAllUserPrimaryProject,
  getByUserId,
  getByUserPrimaryProjectId,
  updateUserPrimaryProject,
} from '../../controller/userPrimaryProject.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  userPrimaryProjectCreateValidator,
  runValidation,
  createUserPrimaryProject
);

router.put(
  '/',
  authMiddleware,
  userPrimaryProjectUpdateValidator,
  runValidation,
  updateUserPrimaryProject
);

router.get('/get-all', authMiddleware, getAllUserPrimaryProject);

router.get(
  '/get/:user_primary_project_id',
  authMiddleware,
  getByUserPrimaryProjectId
);

router.delete(
  '/delete/:user_primary_project_id',
  authMiddleware,
  deleteByUserPrimaryProjectId
);

router.get('/get-by-user-id/:user_id', authMiddleware, getByUserId);

export default router;
