import catchAsync from '../utils/catchAsync';
import * as inventoryService from '../services/inventory.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createInventory = catchAsync(async (req, res) => {
  const methodName = '/createInventory';
  try {
    const inventory = await inventoryService.createInventory(req.body);
    res.send(inventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateInventory = catchAsync(async (req, res) => {
  const methodName = '/updateInventory';
  try {
    const inventory = await inventoryService.updateInventory(req.body);
    res.send(inventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllInventorys = catchAsync(async (req, res) => {
  const methodName = '/getAllInventorys';
  try {
    const inventory = await inventoryService.getAllInventorys();
    res.send(inventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByInventoryId = catchAsync(async (req, res) => {
  const methodName = '/getByInventoryId';
  try {
    const inventory = await inventoryService.getById(req.params.inventory_id);
    res.send(inventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByInventoryId = catchAsync(async (req, res) => {
  const methodName = '/deleteByInventoryId';
  try {
    const inventory = await inventoryService.deleteInventory(
      req.params.inventory_id
    );
    res.send(inventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchInventory = catchAsync(async (req, res) => {
  const methodName = '/searchInventory';
  try {
    const inventory = await inventoryService.searchInventory(req.body);
    res.send(inventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createInventory,
  updateInventory,
  getAllInventorys,
  getByInventoryId,
  deleteByInventoryId,
  searchInventory,
};
