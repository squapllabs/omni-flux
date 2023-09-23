import catchAsync from '../utils/catchAsync';
import * as projectInventoryService from '../services/projectInventory.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createProjectInventory = catchAsync(async (req, res) => {
  const methodName = '/createProjectInventory';
  try {
    const projectInventory =
      await projectInventoryService.createProjectInventory(req.body);
    res.send(projectInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateProjectInventory = catchAsync(async (req, res) => {
  const methodName = '/updateProjectInventory';
  try {
    const projectInventory =
      await projectInventoryService.updateProjectInventory(req.body);
    res.send(projectInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllProjectInventorys = catchAsync(async (req, res) => {
  const methodName = '/getAllProjectInventorys';
  try {
    const projectInventory =
      await projectInventoryService.getAllProjectInventorys();
    res.send(projectInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectInventoryId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectInventoryId';
  try {
    const projectInventory = await projectInventoryService.getById(
      req.params.project_inventory_id
    );
    res.send(projectInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByProjectInventoryId = catchAsync(async (req, res) => {
  const methodName = '/deleteByProjectInventoryId';
  try {
    const projectInventory =
      await projectInventoryService.deleteProjectInventory(
        req.params.project_inventory_id
      );
    res.send(projectInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchProjectInventory = catchAsync(async (req, res) => {
  const methodName = '/searchProjectInventory';
  try {
    const projectInventory =
      await projectInventoryService.searchProjectInventory(req.body);
    res.send(projectInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectId';
  try {
    const projectInventory = await projectInventoryService.getByProjectId(
      req.params.project_id
    );
    res.send(projectInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectIdAndSiteId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectIdAndSiteId';
  try {
    const projectInventory =
      await projectInventoryService.getByProjectIdAndSiteId(
        req.params.project_id,
        req.params.site_id
      );
    res.send(projectInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createProjectInventory,
  updateProjectInventory,
  getAllProjectInventorys,
  getByProjectInventoryId,
  deleteByProjectInventoryId,
  searchProjectInventory,
  getByProjectId,
  getByProjectIdAndSiteId,
};
