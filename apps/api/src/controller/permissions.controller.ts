import catchAsync from '../utils/catchAsync';
import * as permissionsService from '../services/permissions.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createPermission = catchAsync(async (req, res) => {
  const methodName = '/createPermission';
  try {
    const permission = await permissionsService.createPermission(req.body);
    res.send(permission);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updatePermission = catchAsync(async (req, res) => {
  const methodName = '/updatePermission';
  try {
    const permission = await permissionsService.updatePermission(req.body);
    res.send(permission);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllPermissions = catchAsync(async (req, res) => {
  const methodName = '/getAllPermissions';
  try {
    const permission = await permissionsService.getAllPermissions();
    res.send(permission);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPermissionId = catchAsync(async (req, res) => {
  const methodName = '/getByPermissionId';
  try {
    const permission = await permissionsService.getById(
      req.params.permission_id
    );
    res.send(permission);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByPermissionId = catchAsync(async (req, res) => {
  const methodName = '/deleteByPermissionId';
  try {
    const permission = await permissionsService.deletePermission(
      req.params.permission_id
    );
    res.send(permission);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchPermission = catchAsync(async (req, res) => {
  const methodName = '/searchPermission';
  try {
    const permission = await permissionsService.searchPermission(req.body);
    res.send(permission);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPermissionUserId = catchAsync(async (req, res) => {
  const methodName = '/getByPermissionId';
  try {
    const permission = await permissionsService.getByusersId(
      req.params.user_id
    );
    res.send(permission);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  getByPermissionUserId,
  createPermission,
  updatePermission,
  getAllPermissions,
  getByPermissionId,
  deleteByPermissionId,
  searchPermission,
};
