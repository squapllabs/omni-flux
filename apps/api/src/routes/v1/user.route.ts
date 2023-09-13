import express from 'express';
import {
  createUser,
  customFilterUser,
  deleteUser,
  getAllSalesPersonUsers,
  getAllUser,
  getByEmailId,
  getByUserId,
  getChildUsersByParentUserId,
  getDeletedUsers,
  getUserByRoleName,
  searchUser,
  updateStatus,
  updateTwoFactorAuthentication,
  updateUser,
} from '../../controller/user.controller';
import authMiddleware from '../../middleware/auth';
import {
  userCreateValidator,
  userUpdateValidator,
  userUpdateStatusValidator,
} from '../../validations/users';
import { runValidation } from '../../validations/index';
const router = express.Router();

router.post(
  '/',
  authMiddleware,
  userCreateValidator,
  runValidation,
  createUser
);

router.put('/', authMiddleware, userUpdateValidator, runValidation, updateUser);
router.get('/getById/:user_id', authMiddleware, getByUserId);
router.get('/getByEmailId/:email_id', authMiddleware, getByEmailId);
router.get('/getAll/:user_status?', authMiddleware, getAllUser);
router.delete('/delete/:user_id', deleteUser);

router.put(
  '/updateStatus',
  authMiddleware,
  userUpdateStatusValidator,
  runValidation,
  updateStatus
);

router.post('/search', authMiddleware, searchUser);

router.get('/getDeletedUsers', authMiddleware, getDeletedUsers);

router.post('/custom-filter', authMiddleware, customFilterUser);

router.put('/update-two-factor', authMiddleware, updateTwoFactorAuthentication);
router.get('/get-all-sales-persons', authMiddleware, getAllSalesPersonUsers);

router.get(
  '/get-users-by-role-name/:role_name',
  authMiddleware,
  getUserByRoleName
);

router.get(
  '/get-child-users-by-parent-user-id/:parent_user_id',
  authMiddleware,
  getChildUsersByParentUserId
);

export default router;
