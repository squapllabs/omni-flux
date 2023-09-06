import * as bomService from '../services/bom.service';
import catchAsync from '../utils/catchAsync';
import { ErrorHandler, handleError } from '../config/error';
const errorText = 'Error';

const createBom = catchAsync(async (req, res) => {
  const methodName = '/createBom';
  try {
    const bom = await bomService.createBom(req.body);
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateBom = catchAsync(async (req, res) => {
  const methodName = '/updateBom';
  try {
    const bom = await bomService.updateBom(req.body);
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllBom = catchAsync(async (req, res) => {
  const methodName = '/getAllBom';
  try {
    const bom = await bomService.getAllBom();
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByBomId = catchAsync(async (req, res) => {
  const methodName = '/getByBomId';
  try {
    const bom = await bomService.fetchBomById(req.params.bom_id);
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteBom = catchAsync(async (req, res) => {
  const methodName = '/deleteBom';
  try {
    const bom = await bomService.deleteBomById(req.params.bom_id);
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByCategorySubCatAndSubSubCatId = catchAsync(async (req, res) => {
  const methodName = '/getByCategorySubCatAndSubSubCatId';
  try {
    const bom = await bomService.getByCategorySubCatAndSubSubCatId(req.body);
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const fetchEntireDataByBomId = catchAsync(async (req, res) => {
  const methodName = '/getEntireData';
  try {
    const bom = await bomService.getEntireDataByBomId(req.params.bom_id);
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const addBulkBom = catchAsync(async (req, res) => {
  const methodName = '/addBulkBom';
  try {
    const bom = await bomService.addBulkBom(req.body);
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBomBySubCategoryIdAndBomType = catchAsync(async (req, res) => {
  const methodName = '/getBomBySubCategoryIdAndBomType';
  try {
    const bom = await bomService.getBomBySubCategoryIdAndBomType(
      req.params.sub_category_id,
      req.params.bom_type
    );
    res.send(bom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBomTotalBySubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getBomTotalBySubCategoryId';
  try {
    const bom = await bomService.getBomTotalBySubCategoryId(
      req.params.sub_category_id
    );
    res.send(bom);
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
  fetchEntireDataByBomId,
  addBulkBom,
  getBomBySubCategoryIdAndBomType,
  getBomTotalBySubCategoryId,
};
