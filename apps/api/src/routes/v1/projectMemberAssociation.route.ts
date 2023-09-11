import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  projectMemberAssociationCreateValidator,
  projectMemberAssociationUpdateValidator,
} from '../../validations/projectMemberAssociation';
import { runValidation } from '../../validations/index';
import {
  createProjectMemberAssociation,
  deleteByProjectMemberAssociationId,
  getAllProjectMemberAssociation,
  getByProjectId,
  getByProjectIdAndRoleType,
  getByProjectIdAndUserId,
  getByProjectMemberAssociationId,
  search,
  updateProjectMemberAssociation,
} from '../../controller/projectMemberAssociation.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  projectMemberAssociationCreateValidator,
  runValidation,
  createProjectMemberAssociation
);

router.put(
  '/',
  authMiddleware,
  projectMemberAssociationUpdateValidator,
  runValidation,
  updateProjectMemberAssociation
);

router.get('/get-all', authMiddleware, getAllProjectMemberAssociation);

router.get(
  '/get/:project_member_association_id',
  authMiddleware,
  getByProjectMemberAssociationId
);

router.delete(
  '/delete/:project_member_association_id',
  authMiddleware,
  deleteByProjectMemberAssociationId
);

router.get(
  '/get-by-project-id-and-user-id/:project_id/:user_id',
  authMiddleware,
  getByProjectIdAndUserId
);

router.get('/get-by-project-id/:project_id', authMiddleware, getByProjectId);

router.post('/search', authMiddleware, search);

router.get(
  '/get-by-project-id-and-role-name/:project_id/:role_name',
  authMiddleware,
  getByProjectIdAndRoleType
);

export default router;
