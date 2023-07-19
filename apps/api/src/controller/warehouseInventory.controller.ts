import catchAsync from '../utils/catchAsync';
import * as warehouseInventoryService from '../services/warehouseInventory.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createWarehouseInventory = catchAsync(async (req, res) => {
  const methodName = '/createWarehouseInventory';
  try {
    const warehouseInventory =
      await warehouseInventoryService.createWarehouseInventory(req.body);
    res.send(warehouseInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateWarehouseInventory = catchAsync(async (req, res) => {
  const methodName = '/updateWarehouseInventory';
  try {
    const warehouseInventory =
      await warehouseInventoryService.updateWarehouseInventory(req.body);
    res.send(warehouseInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllWarehouseInventory = catchAsync(async (req, res) => {
  const methodName = '/getAllWarehouseInventory';
  try {
    const warehouseInventory =
      await warehouseInventoryService.getAllWarehouseInventory();
    res.send(warehouseInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByWarehouseInventoryId = catchAsync(async (req, res) => {
  const methodName = '/getByWarehouseInventoryId';
  try {
    const warehouseInventory = await warehouseInventoryService.getById(
      req.params.warehouse_inventory_id
    );
    res.send(warehouseInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByWarehouseInventoryId = catchAsync(async (req, res) => {
  const methodName = '/deleteByWarehouseInventoryId';
  try {
    const warehouseInventory =
      await warehouseInventoryService.deleteWarehouseInventory(
        req.params.warehouse_inventory_id
      );
    res.send(warehouseInventory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createWarehouseInventory,
  updateWarehouseInventory,
  getAllWarehouseInventory,
  getByWarehouseInventoryId,
  deleteByWarehouseInventoryId,
};
