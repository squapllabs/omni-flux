import catchAsync from '../utils/catchAsync';
import * as projectSiteService from '../services/projectSite.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const getByProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectId';
  try {
    const projectSite = await projectSiteService.getByProjectId(
      req.params.project_id
    );
    res.send(projectSite);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const createProjectSite = catchAsync(async (req, res) => {
  const methodName = '/createProjectSite';
  try {
    const projectSite = await projectSiteService.createProjectSite(req.body);
    res.send(projectSite);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateProjectSite = catchAsync(async (req, res) => {
  const methodName = '/updateProjectSite';
  try {
    const projectSite = await projectSiteService.updateProjectSite(req.body);
    res.send(projectSite);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllProjectSites = catchAsync(async (req, res) => {
  const methodName = '/getAllProjectSites';
  try {
    const projectSite = await projectSiteService.getAllProjectSites();
    res.send(projectSite);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectSiteId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectSiteId';
  try {
    const projectSite = await projectSiteService.getByProjectSiteId(
      req.params.project_site_id
    );
    res.send(projectSite);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchProjectSite = catchAsync(async (req, res) => {
  const methodName = '/searchProjectSite';
  try {
    const projectSite = await projectSiteService.searchProjectSite(req.body);
    res.send(projectSite);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  getByProjectId,
  createProjectSite,
  updateProjectSite,
  getAllProjectSites,
  getByProjectSiteId,
  searchProjectSite,
};
