import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  roleCreateValidator,
  roleUpdateValidator,
} from '../../validations/role';
import { runValidation } from '../../validations/index';
import {
  createRole,
  deleteByRoleId,
  getAllRoles,
  getByRoleId,
  updateRole,
} from '../../controller/role.controller';
const router = express.Router();

router.post(
  '/',
  authMiddleware,
  roleCreateValidator,
  runValidation,
  createRole
);

router.put('/', authMiddleware, roleUpdateValidator, runValidation, updateRole);

router.get('/getAll', authMiddleware, getAllRoles);

router.get('/get/:role_id', authMiddleware, getByRoleId);

router.delete('/delete/:role_id', authMiddleware, deleteByRoleId);

export default router;
