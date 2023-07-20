import catchAsync from '../utils/catchAsync';
import * as warehouseService from '../services/warehouse.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createWarehouse = catchAsync(async (req, res) => {
  const methodName = '/createWarehouse';
  try {
    const warehouse = await warehouseService.createWarehouse(req.body);
    res.send(warehouse);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateWarehouse = catchAsync(async (req, res) => {
  const methodName = '/updateWarehouse';
  try {
    const warehouse = await warehouseService.updateWarehouse(req.body);
    res.send(warehouse);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllWarehouse = catchAsync(async (req, res) => {
  const methodName = '/getAllWarehouse';
  try {
    const warehouse = await warehouseService.getAllWarehouse();
    res.send(warehouse);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByWarehouseId = catchAsync(async (req, res) => {
  const methodName = '/getByWarehouseId';
  try {
    const warehouse = await warehouseService.getById(req.params.warehouse_id);
    res.send(warehouse);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByWarehouseId = catchAsync(async (req, res) => {
  const methodName = '/deleteByWarehouseId';
  try {
    const warehouse = await warehouseService.deleteWarehouse(
      req.params.warehouse_id
    );
    res.send(warehouse);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createWarehouse,
  updateWarehouse,
  getAllWarehouse,
  getByWarehouseId,
  deleteByWarehouseId,
};
