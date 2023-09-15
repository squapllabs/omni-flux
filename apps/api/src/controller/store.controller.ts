import catchAsync from '../utils/catchAsync';
import * as storeService from '../services/store.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createStore = catchAsync(async (req, res) => {
  const methodName = '/createStore';
  try {
    const store = await storeService.createStore(req.body);
    res.send(store);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateStore = catchAsync(async (req, res) => {
  const methodName = '/updateStore';
  try {
    const store = await storeService.updateStore(req.body);
    res.send(store);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllStore = catchAsync(async (req, res) => {
  const methodName = '/getAllStore';
  try {
    const store = await storeService.getAllStore();
    res.send(store);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByStoreId = catchAsync(async (req, res) => {
  const methodName = '/getByStoreId';
  try {
    const store = await storeService.getById(req.params.store_id);
    res.send(store);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByStoreId = catchAsync(async (req, res) => {
  const methodName = '/deleteByStoreId';
  try {
    const store = await storeService.deleteStore(req.params.store_id);
    res.send(store);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchStore = catchAsync(async (req, res) => {
  const methodName = '/searchStore';
  try {
    const store = await storeService.searchStore(req.body);
    res.send(store);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createStore,
  updateStore,
  getAllStore,
  getByStoreId,
  deleteByStoreId,
  searchStore,
};
