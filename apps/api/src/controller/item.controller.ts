import catchAsync from '../utils/catchAsync';
import * as itemService from '../services/item.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const addItem = catchAsync(async (req, res) => {
  const methodName = '/addItem';
  try {
    const result = await itemService.addItem(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const addBulkItems = catchAsync(async (req, res) => {
  const methodName = '/addBulkItems';
  try {
    const result = await itemService.addBulkItems(req);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllItem = catchAsync(async (req, res) => {
  const methodName = '/getAllItem';
  try {
    const item = await itemService.getAllItem(req.body);
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllItemBySearch = catchAsync(async (req, res) => {
  const methodName = '/getAllItemBySearch';
  try {
    const result = await itemService.getAllItemBySearch(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByItemId = catchAsync(async (req, res) => {
  const methodName = '/getByItemId';
  try {
    const item = await itemService.getById(req.params.item_id);
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByItemId = catchAsync(async (req, res) => {
  const methodName = '/deleteByItemId';
  try {
    const item = await itemService.deleteItem(req.params.item_id);
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateItem = catchAsync(async (req, res) => {
  const methodName = '/updateItem';
  try {
    const item = await itemService.updateItem(req.body);
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllItemData = catchAsync(async (req, res) => {
  const methodName = '/getAllItemData';
  try {
    const item = await itemService.getAllItemData();
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const search = catchAsync(async (req, res) => {
  const methodName = '/search';
  try {
    const item = await itemService.searchItem(req.body);
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const checkDuplicateName = catchAsync(async (req, res) => {
  const methodName = '/checkDuplicateName';
  try {
    const item = await itemService.getByItemName(req.params.item_name);
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByIndentRequestId = catchAsync(async (req, res) => {
  const methodName = '/getByIndentRequestId';
  try {
    const item = await itemService.getByIndentRequestId(
      req.params.indent_request_id
    );
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByCode = catchAsync(async (req, res) => {
  const methodName = '/getByCode';
  try {
    const item = await itemService.getByCode(req.params.code);
    res.send(item);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  addItem,
  deleteByItemId,
  getAllItem,
  getByItemId,
  updateItem,
  addBulkItems,
  getAllItemBySearch,
  getAllItemData,
  search,
  checkDuplicateName,
  getByIndentRequestId,
  getByCode,
};
