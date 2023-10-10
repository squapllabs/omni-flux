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

const checkDuplicateSubCategoryName = catchAsync(async (req, res) => {
  const methodName = '/checkDuplicateSubCategoryName';
  try {
    const subCategory = await subCategoryService.checkDuplicateSubCategoryName(
      req.params.sub_category_name,
      req.params.category_id
    );
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllInActiveSubCategories = catchAsync(async (req, res) => {
  const methodName = '/getAllInActiveSubCategories';
  try {
    const subCategory = await subCategoryService.getAllInActiveSubCategories();
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchSubCategory = catchAsync(async (req, res) => {
  const methodName = '/searchSubCategory';
  try {
    const subCategory = await subCategoryService.searchSubCategory(req.body);
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getByCategoryId';
  try {
    const subCategory = await subCategoryService.getByCategoryId(
      req.params.category_id,
      req.params.bom_configuration_id
    );
    res.send(subCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByParentSubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getByParentSubCategoryId';
  try {
    const subCategory = await subCategoryService.getByParentSubCategoryId(
      req.params.parent_sub_category_id
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
  checkDuplicateSubCategoryName,
  getAllInActiveSubCategories,
  searchSubCategory,
  getByCategoryId,
  getByParentSubCategoryId,
};
