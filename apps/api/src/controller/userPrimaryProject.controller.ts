import catchAsync from '../utils/catchAsync';
import * as userPrimaryProjectService from '../services/userPrimaryProject.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createUserPrimaryProject = catchAsync(async (req, res) => {
  const methodName = '/createUserPrimaryProject';
  try {
    const userPrimaryProject =
      await userPrimaryProjectService.createUserPrimaryProject(req.body);
    res.send(userPrimaryProject);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateUserPrimaryProject = catchAsync(async (req, res) => {
  const methodName = '/updateUserPrimaryProject';
  try {
    const userPrimaryProject =
      await userPrimaryProjectService.updateUserPrimaryProject(req.body);
    res.send(userPrimaryProject);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllUserPrimaryProject = catchAsync(async (req, res) => {
  const methodName = '/getAllUserPrimaryProject';
  try {
    const userPrimaryProject =
      await userPrimaryProjectService.getAllUserPrimaryProject();
    res.send(userPrimaryProject);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByUserPrimaryProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByUserPrimaryProjectId';
  try {
    const userPrimaryProject = await userPrimaryProjectService.getById(
      req.params.user_primary_project_id
    );
    res.send(userPrimaryProject);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByUserPrimaryProjectId = catchAsync(async (req, res) => {
  const methodName = '/deleteByUserPrimaryProjectId';
  try {
    const userPrimaryProject =
      await userPrimaryProjectService.deleteUserPrimaryProject(
        req.params.user_primary_project_id
      );
    res.send(userPrimaryProject);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByUserId = catchAsync(async (req, res) => {
  const methodName = '/getByUserId';
  try {
    const userPrimaryProject = await userPrimaryProjectService.getByUserId(
      req.params.user_id
    );
    res.send(userPrimaryProject);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createUserPrimaryProject,
  updateUserPrimaryProject,
  getAllUserPrimaryProject,
  getByUserPrimaryProjectId,
  deleteByUserPrimaryProjectId,
  getByUserId,
};
