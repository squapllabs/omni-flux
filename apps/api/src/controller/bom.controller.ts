import * as bomService from '../services/bom.service';
import catchAsync from '../utils/catchAsync';
import { ErrorHandler, handleError } from '../config/error';
const errorText = 'Error';

const createBom = catchAsync(async (req, res) => {
  const methodName = '/createBom';
  try {
    const result = await bomService.createBom(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateBom = catchAsync(async (req, res) => {
  const methodName = '/updateBom';
  try {
    const result = await bomService.updateBom(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllBom = catchAsync(async (req, res) => {
  const methodName = '/getAllBom';
  try {
    const category = await bomService.getAllBom();
    res.send(category);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByBomId = catchAsync(async (req, res) => {
  const methodName = '/getByBomId';
  try {
    const result = await bomService.fetchBomById(req.params.bom_id);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteBom = catchAsync(async (req, res) => {
  const methodName = '/deleteBom';
  try {
    const result = await bomService.deleteBomById(req.params.bom_id);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByCategorySubCatAndSubSubCatId = catchAsync(async (req, res) => {
  const methodName = '/getByCategorySubCatAndSubSubCatId';
  try {
    const result = await bomService.getByCategorySubCatAndSubSubCatId(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createBom,
  updateBom,
  getByBomId,
  deleteBom,
  getAllBom,
  getByCategorySubCatAndSubSubCatId,
};
