import catchAsync from '../utils/catchAsync';
import * as roleService from '../services/role.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createRole = catchAsync(async (req, res) => {
  const methodName = '/createRole';
  try {
    const role = await roleService.createRole(req.body);
    res.send(role);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateRole = catchAsync(async (req, res) => {
  const methodName = '/updateRole';
  try {
    const role = await roleService.updateRole(req.body);
    res.send(role);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllRoles = catchAsync(async (req, res) => {
  const methodName = '/getAllRoles';
  try {
    const role = await roleService.getAllRoles();
    res.send(role);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByRoleId = catchAsync(async (req, res) => {
  const methodName = '/getByRoleId';
  try {
    const role = await roleService.getById(req.params.role_id);
    res.send(role);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByRoleId = catchAsync(async (req, res) => {
  const methodName = '/deleteByRoleId';
  try {
    const role = await roleService.deleteRole(req.params.role_id);
    res.send(role);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { createRole, updateRole, getAllRoles, getByRoleId, deleteByRoleId };
