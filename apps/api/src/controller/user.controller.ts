/* eslint-disable @typescript-eslint/no-explicit-any */
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

const userLogin = catchAsync(async (req, res) => {
  const methodName = '/userLogin';
  try {
    const result = await userService.userLogin(req.body, res);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllUser = catchAsync(async (req, res) => {
  const methodName = '/getAll';
  try {
    const users = await userService.getAllUser();
    res.send(users);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const userLogOut = catchAsync(async (req, res) => {
  const methodName = '/userLogOut';
  try {
    const result = await userService.userLogOut(req, res);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createUser,
  getByUserId,
  getByEmailId,
  userLogin,
  getAllUser,
  userLogOut,
};
