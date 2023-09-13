import catchAsync from '../utils/catchAsync';
import * as projectService from '../services/project.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createProject = catchAsync(async (req, res) => {
  const methodName = '/createProject';
  try {
    const project = await projectService.createProject(req.body);
    res.send(project);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateProject = catchAsync(async (req, res) => {
  const methodName = '/updateProject';
  try {
    const project = await projectService.updateProject(req.body);
    res.send(project);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllProject = catchAsync(async (req, res) => {
  const methodName = '/getAllProject';
  try {
    const project = await projectService.getAllProject();
    res.send(project);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectId';
  try {
    const project = await projectService.getById(req.params.project_id);
    res.send(project);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByPojectId = catchAsync(async (req, res) => {
  const methodName = '/deleteByPojectId';
  try {
    const project = await projectService.deleteProject(req.params.project_id);
    res.send(project);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchProject = catchAsync(async (req, res) => {
  const methodName = '/searchProject';
  try {
    const project = await projectService.searchProject(req.body);
    res.send(project);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const checkDuplicateCode = catchAsync(async (req, res) => {
  const methodName = '/checkDuplicateCode';
  try {
    const project = await projectService.getByCode(req.params.code);
    res.send(project);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createProject,
  updateProject,
  getAllProject,
  getByProjectId,
  deleteByPojectId,
  searchProject,
  checkDuplicateCode,
};
