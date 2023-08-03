import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  projectWorkbreakDownCreateValidator,
  projectWorkbreakDownUpdateValidator,
} from '../../validations/projectWorkbreakDown';
import { runValidation } from '../../validations/index';
import {
  createProjectWorkbreakDown,
  deleteByprojectWorkbreakDownId,
  getAllParentProjectWorkbreakDown,
  getAllProjectWorkbreakDown,
  getByCode,
  getByProjectWorkbreakDownId,
  searchProjectWorkbreakDown,
  updateProjectWorkbreakDown,
} from '../../controller/projectWorkbreakDown.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  projectWorkbreakDownCreateValidator,
  runValidation,
  createProjectWorkbreakDown
);

router.put(
  '/',
  authMiddleware,
  projectWorkbreakDownUpdateValidator,
  runValidation,
  updateProjectWorkbreakDown
);

router.get('/get-all', authMiddleware, getAllProjectWorkbreakDown);

router.get(
  '/get/:project_workbreak_down_id',
  authMiddleware,
  getByProjectWorkbreakDownId
);

router.delete(
  '/delete/:project_workbreak_down_id',
  authMiddleware,
  deleteByprojectWorkbreakDownId
);

router.post('/get-by-code', authMiddleware, getByCode);

router.post('/search', authMiddleware, searchProjectWorkbreakDown);

router.get(
  '/get-all-parent-data',
  authMiddleware,
  getAllParentProjectWorkbreakDown
);

export default router;
