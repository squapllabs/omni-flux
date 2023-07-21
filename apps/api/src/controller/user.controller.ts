import catchAsync from '../utils/catchAsync';
import * as userService from '../services/user.service';
import { handleError, ErrorHandler } from '../config/error';

const errorText = 'Error';

const createUser = catchAsync(async (req, res) => {
  const methodName = '/createUser';
  try {
    const result = await userService.createUser(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateUser = catchAsync(async (req, res) => {
  const methodName = '/updateUser';
  try {
    const result = await userService.updateUser(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByUserId = catchAsync(async (req, res) => {
  const methodName = '/getById';
  try {
    const result = await userService.getById(req.params.user_id);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByEmailId = catchAsync(async (req, res) => {
  const methodName = '/getByEmailId';
  try {
    const result = await userService.getByEmailId(req.params.email_id);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});


const getAllUser = catchAsync(async (req, res) => {
  const methodName = '/getAll';
  try {
    const users = await userService.getAllUser(req.params.user_status);
    res.send(users);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteUser = catchAsync(async (req, res) => {
  const methodName = '/deleteUser';
  try {
    const result = await userService.deleteUser(req.params.user_id);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateStatus = catchAsync(async (req, res) => {
  const methodName = '/updateStatus';
  try {
    const result = await userService.updateStatus(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchUser = catchAsync(async (req, res) => {
  const methodName = '/searchUser';
  try {
    const result = await userService.searchUser(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getDeletedUsers = catchAsync(async (req, res) => {
  const methodName = '/getDeletedUsers';
  try {
    const result = await userService.getDeletedUsers();
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const customFilterUser = catchAsync(async (req, res) => {
  const methodName = '/customFilterUser';
  try {
    const result = await userService.customFilterUser(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createUser,
  updateUser,
  getByUserId,
  getByEmailId,
  getAllUser,
  deleteUser,
  updateStatus,
  searchUser,
<<<<<<< HEAD
  getDeletedUsers,
  customFilterUser,
=======
  getDeletedUsers
>>>>>>> 510565ae7e568e0cf7a291edaa86a90dc534d03c
};
