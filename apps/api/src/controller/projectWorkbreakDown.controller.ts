import catchAsync from '../utils/catchAsync';
import * as projectWorkbreakDownService from '../services/projectWorkbreakDown.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createProjectWorkbreakDown = catchAsync(async (req, res) => {
  const methodName = '/createProjectWorkbreakDown';
  try {
    const projectWorkbreakDown =
      await projectWorkbreakDownService.createProjectWorkbreakDown(req.body);
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateProjectWorkbreakDown = catchAsync(async (req, res) => {
  const methodName = '/updateProjectWorkbreakDown';
  try {
    const projectWorkbreakDown =
      await projectWorkbreakDownService.updateProjectWorkbreakDown(req.body);
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllProjectWorkbreakDown = catchAsync(async (req, res) => {
  const methodName = '/getAllProjectWorkbreakDown';
  try {
    const projectWorkbreakDown =
      await projectWorkbreakDownService.getAllProjectWorkbreakDown();
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectWorkbreakDownId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectWorkbreakDownId';
  try {
    const projectWorkbreakDown = await projectWorkbreakDownService.getById(
      req.params.project_workbreak_down_id
    );
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByprojectWorkbreakDownId = catchAsync(async (req, res) => {
  const methodName = '/deleteByprojectWorkbreakDownId';
  try {
    const projectWorkbreakDown =
      await projectWorkbreakDownService.deleteProjectWorkbreakDown(
        req.params.project_workbreak_down_id
      );
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByCode = catchAsync(async (req, res) => {
  const methodName = '/getByCode';
  try {
    const projectWorkbreakDown = await projectWorkbreakDownService.getByCode(
      req.body
    );
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchProjectWorkbreakDown = catchAsync(async (req, res) => {
  const methodName = '/searchProjectWorkbreakDown';
  try {
    const projectWorkbreakDown =
      await projectWorkbreakDownService.searchProjectWorkbreakDown(req.body);
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllParentProjectWorkbreakDown = catchAsync(async (req, res) => {
  const methodName = '/getAllParentProjectWorkbreakDown';
  try {
    const projectWorkbreakDown =
      await projectWorkbreakDownService.getAllParentProjectWorkbreakDown();
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const checkDuplicateCode = catchAsync(async (req, res) => {
  const methodName = '/checkDuplicateCode';
  try {
    const projectWorkbreakDown =
      await projectWorkbreakDownService.checkDuplicateCode(
        req.params.project_workbreak_down_code
      );
    res.send(projectWorkbreakDown);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createProjectWorkbreakDown,
  updateProjectWorkbreakDown,
  getAllProjectWorkbreakDown,
  getByProjectWorkbreakDownId,
  deleteByprojectWorkbreakDownId,
  getByCode,
  searchProjectWorkbreakDown,
  getAllParentProjectWorkbreakDown,
  checkDuplicateCode,
};
