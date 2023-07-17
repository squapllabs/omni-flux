import catchAsync from '../utils/catchAsync';
import * as subCategoryService from '../services/subCategory.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createSubCategory = catchAsync(async (req, res) => {
  const methodName = '/createSubCategory';
  try {
    const subCategory = await subCategoryService.createSubCategory(req.body);
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateSubCategory = catchAsync(async (req, res) => {
  const methodName = '/updateSubCategory';
  try {
    const subCategory = await subCategoryService.updateSubCategory(req.body);
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllSubCategory = catchAsync(async (req, res) => {
  const methodName = '/getAllSubCategory';
  try {
    const subCategory = await subCategoryService.getAllSubCategory();
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBySubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getBySubCategoryId';
  try {
    const subCategory = await subCategoryService.getById(
      req.params.sub_category_id
    );
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteBySubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/deleteBySubCategoryId';
  try {
    const subCategory = await subCategoryService.deleteSubCategory(
      req.params.sub_category_id
    );
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createSubCategory,
  updateSubCategory,
  getAllSubCategory,
  getBySubCategoryId,
  deleteBySubCategoryId,
};
