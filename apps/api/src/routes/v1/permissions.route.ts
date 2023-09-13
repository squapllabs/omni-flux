import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  permissionsCreateValidator,
  permissionsUpdateValidator,
} from '../../validations/permissions';
import { runValidation } from '../../validations/index';
import {
  createPermission,
  deleteByPermissionId,
  getAllPermissions,
  getByPermissionId,
  searchPermission,
  updatePermission,
  getByPermissionUserId,

} from '../../controller/permissions.controller';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  permissionsCreateValidator,
  runValidation,
  createPermission
);

router.put(
  '/',
  authMiddleware,
  permissionsUpdateValidator,
  runValidation,
  updatePermission
);

router.get('/get-all',  getAllPermissions);

router.get('/get/:permission_id', authMiddleware, getByPermissionId);

router.delete('/delete/:permission_id', authMiddleware, deleteByPermissionId);

router.post('/search', authMiddleware, searchPermission);

router.get('/get/permissions/:user_id',getByPermissionUserId);

export default router;
