import * as bomDetailService from '../services/bomDetail.service';
import catchAsync from '../utils/catchAsync';
import { ErrorHandler, handleError } from '../config/error';
const errorText = 'Error';

const createBom = catchAsync(async (req, res) => {
  const methodName = '/createBom';
  try {
    const bomDetail = await bomDetailService.createBom(req.body);
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateBom = catchAsync(async (req, res) => {
  const methodName = '/updateBom';
  try {
    const bomDetail = await bomDetailService.updateBom(req.body);
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllBom = catchAsync(async (req, res) => {
  const methodName = '/getAllBom';
  try {
    const bomDetail = await bomDetailService.getAllBom();
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByBomId = catchAsync(async (req, res) => {
  const methodName = '/getByBomId';
  try {
    const bomDetail = await bomDetailService.fetchBomById(req.params.bom_id);
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteBom = catchAsync(async (req, res) => {
  const methodName = '/deleteBom';
  try {
    const bomDetail = await bomDetailService.deleteBomById(req.params.bom_id);
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByCategorySubCatAndSubSubCatId = catchAsync(async (req, res) => {
  const methodName = '/getByCategorySubCatAndSubSubCatId';
  try {
    const bomDetail = await bomDetailService.getByCategorySubCatAndSubSubCatId(
      req.body
    );
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const fetchEntireDataByBomId = catchAsync(async (req, res) => {
  const methodName = '/getEntireData';
  try {
    const bomDetail = await bomDetailService.getEntireDataByBomId(
      req.params.bom_id
    );
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const addBulkBom = catchAsync(async (req, res) => {
  const methodName = '/addBulkBom';
  try {
    const bomDetail = await bomDetailService.addBulkBom(req.body);
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBomBySubCategoryIdAndBomType = catchAsync(async (req, res) => {
  const methodName = '/getBomBySubCategoryIdAndBomType';
  try {
    const bomDetail = await bomDetailService.getBomBySubCategoryIdAndBomType(
      req.params.sub_category_id,
      req.params.bom_type
    );
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBomTotalBySubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getBomTotalBySubCategoryId';
  try {
    const bomDetail = await bomDetailService.getBomTotalBySubCategoryId(
      req.params.sub_category_id
    );
    res.send(bomDetail);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBySubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getBySubCategoryId';
  try {
    const bomDetail = await bomDetailService.getBySubCategoryId(
      req.params.sub_category_id
    );
    res.send(bomDetail);
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
  getBySubCategoryId,
};
