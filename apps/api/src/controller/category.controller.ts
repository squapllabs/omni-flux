import catchAsync from '../utils/catchAsync';
import * as categoryService from '../services/category.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createCategory = catchAsync(async (req, res) => {
  const methodName = '/createCategory';
  try {
    const category = await categoryService.createCategory(req.body);
    res.send(category);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateCategory = catchAsync(async (req, res) => {
  const methodName = '/updateCategory';
  try {
    const category = await categoryService.updateCategory(req.body);
    res.send(category);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllCategory = catchAsync(async (req, res) => {
  const methodName = '/getAllCategory';
  try {
    const category = await categoryService.getAllCategory();
    res.send(category);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByCategoryId = catchAsync(async (req, res) => {
  const methodName = '/getByCategoryId';
  try {
    const category = await categoryService.getById(req.params.category_id);
    res.send(category);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByCategoryId = catchAsync(async (req, res) => {
  const methodName = '/deleteByCategoryId';
  try {
    const category = await categoryService.deleteCategory(
      req.params.category_id
    );
    res.send(category);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createCategory,
  updateCategory,
  getAllCategory,
  getByCategoryId,
  deleteByCategoryId,
};
