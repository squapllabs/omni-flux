import catchAsync from '../utils/catchAsync';
import * as subSubCategoryService from '../services/subSubCategory.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createSubSubCategory = catchAsync(async (req, res) => {
  const methodName = '/createSubSubCategory';
  try {
    const subSubCategory = await subSubCategoryService.createSubSubCategory(
      req.body
    );
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateSubSubCategory = catchAsync(async (req, res) => {
  const methodName = '/updateSubSubCategory';
  try {
    const subSubCategory = await subSubCategoryService.updateSubSubCategory(
      req.body
    );
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllSubSubCategory = catchAsync(async (req, res) => {
  const methodName = '/getAllSubSubCategory';
  try {
    const subSubCategory = await subSubCategoryService.getAllSubSubCategory();
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBySubSubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getBySubSubCategoryId';
  try {
    const subSubCategory = await subSubCategoryService.getById(
      req.params.sub_sub_category_id
    );
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteBySubSubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/deleteBySubSubCategoryId';
  try {
    const subSubCategory = await subSubCategoryService.deleteSubSubCategory(
      req.params.sub_sub_category_id
    );
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const checkDuplicateSubSubCategoryName = catchAsync(async (req, res) => {
  const methodName = '/checkDuplicateSubSubCategoryName';
  try {
    const subSubCategory =
      await subSubCategoryService.checkDuplicateSubSubCategoryName(
        req.params.sub_sub_category_name,
        req.params.sub_category_id
      );
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllInActiveSubSubCategories = catchAsync(async (req, res) => {
  const methodName = '/getAllInActiveSubSubCategories';
  try {
    const subSubCategory =
      await subSubCategoryService.getAllInActiveSubSubCategories();
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchSubSubCategory = catchAsync(async (req, res) => {
  const methodName = '/searchSubSubCategory';
  try {
    const subSubCategory = await subSubCategoryService.searchSubSubCategory(
      req.body
    );
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBySubCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getBySubCategoryId';
  try {
    const subSubCategory = await subSubCategoryService.getBySubCategoryId(
      req.params.sub_category_id
    );
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllParentData = catchAsync(async (req, res) => {
  const methodName = '/getAllParentData';
  try {
    const subSubCategory = await subSubCategoryService.getAllParentData();
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getChildDataByParentSubSubCatId = catchAsync(async (req, res) => {
  const methodName = '/getChildDataByParentSubSubCatId';
  try {
    const subSubCategory =
      await subSubCategoryService.getChildDataByParentSubSubCatId(
        req.params.parent_sub_sub_category_id
      );
    res.send(subSubCategory);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createSubSubCategory,
  updateSubSubCategory,
  getAllSubSubCategory,
  getBySubSubCategoryId,
  deleteBySubSubCategoryId,
  checkDuplicateSubSubCategoryName,
  getAllInActiveSubSubCategories,
  searchSubSubCategory,
  getBySubCategoryId,
  getAllParentData,
  getChildDataByParentSubSubCatId,
};
