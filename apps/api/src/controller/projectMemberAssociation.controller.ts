import catchAsync from '../utils/catchAsync';
import * as projectMemberAssociationService from '../services/projectMemberAssociation.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createProjectMemberAssociation = catchAsync(async (req, res) => {
  const methodName = '/createProjectMemberAssociation';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.createProjectMemberAssociation(
        req.body
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateProjectMemberAssociation = catchAsync(async (req, res) => {
  const methodName = '/updateProjectMemberAssociation';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.updateProjectMemberAssociation(
        req.body
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllProjectMemberAssociation = catchAsync(async (req, res) => {
  const methodName = '/getAllProjectMemberAssociation';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.getAllProjectMemberAssociation();
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectMemberAssociationId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectMemberAssociationId';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.getById(
        req.params.project_member_association_id
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByProjectMemberAssociationId = catchAsync(async (req, res) => {
  const methodName = '/deleteByProjectMemberAssociationId';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.deleteProjectMemberAssociation(
        req.params.project_member_association_id
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectIdAndUserId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectIdAndUserId';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.getByProjectIdAndUserId(
        req.params.project_id,
        req.params.user_id
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectId';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.getByProjectId(
        req.params.project_id
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const search = catchAsync(async (req, res) => {
  const methodName = '/search';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.searchProjectMemberAssociation(
        req.body
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectIdAndRoleType = catchAsync(async (req, res) => {
  const methodName = '/getByProjectIdAndRoleType';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.getByProjectIdAndRoleType(
        req.params.project_id,
        req.params.role_name
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByUserIdAndProjectRoleId = catchAsync(async (req, res) => {
  const methodName = '/getByUserIdAndProjectRoleId';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.getByUserIdAndProjectRoleId(
        req.params.user_id,
        req.params.project_role_id
      );
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchByUserId = catchAsync(async (req, res) => {
  const methodName = '/searchByUserId';
  try {
    const projectMemberAssociation =
      await projectMemberAssociationService.searchByUserId(req.body);
    res.send(projectMemberAssociation);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createProjectMemberAssociation,
  updateProjectMemberAssociation,
  getAllProjectMemberAssociation,
  getByProjectMemberAssociationId,
  deleteByProjectMemberAssociationId,
  getByProjectIdAndUserId,
  getByProjectId,
  search,
  getByProjectIdAndRoleType,
  getByUserIdAndProjectRoleId,
  searchByUserId,
};
