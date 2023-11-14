import catchAsync from '../utils/catchAsync';
import * as expenseService from '../services/expense.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createExpense = catchAsync(async (req, res) => {
  const methodName = '/createExpense';
  try {
    const expense = await expenseService.createExpense(req.body);
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateExpense = catchAsync(async (req, res) => {
  const methodName = '/updateExpense';
  try {
    const expense = await expenseService.updateExpense(req.body);
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllExpense = catchAsync(async (req, res) => {
  const methodName = '/getAllExpense';
  try {
    const expense = await expenseService.getAllExpense();
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByExpenseId = catchAsync(async (req, res) => {
  const methodName = '/getByExpenseId';
  try {
    const expense = await expenseService.getById(req.params.expense_id);
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByExpenseId = catchAsync(async (req, res) => {
  const methodName = '/deleteByExpenseId';
  try {
    const expense = await expenseService.deleteExpense(req.params.expense_id);
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchExpense = catchAsync(async (req, res) => {
  const methodName = '/searchExpense';
  try {
    const expense = await expenseService.searchExpense(req.body);
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectIdAndSiteId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectIdAndSiteId';
  try {
    const expense = await expenseService.getByProjectIdAndSiteId(
      req.params.project_id,
      req.params.site_id
    );
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getExpenseDetailsByExpenseId = catchAsync(async (req, res) => {
  const methodName = '/getExpenseDetailsExpenseId';
  try {
    const expenseDetails = await expenseService.getExpenseDetailsByExpenseId(
      req.params.expense_id
    );
    res.send(expenseDetails);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateStatus = catchAsync(async (req, res) => {
  const methodName = '/updateStatus';
  try {
    const expense = await expenseService.updateStatus(req.body);
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByExpenseCode = catchAsync(async (req, res) => {
  const methodName = '/getByExpenseCode';
  try {
    const expense = await expenseService.getByExpenseCode(
      req.params.expense_code
    );
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const addIndependentExpense = catchAsync(async (req, res) => {
  const methodName = '/addIndependentExpense';
  try {
    const expense = await expenseService.addIndependentExpense(req.body);
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateIndependentExpense = catchAsync(async (req, res) => {
  const methodName = '/updateIndependentExpense';
  try {
    const expense = await expenseService.updateIndependentExpense(req.body);
    res.send(expense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createExpense,
  updateExpense,
  getAllExpense,
  getByExpenseId,
  deleteByExpenseId,
  searchExpense,
  getByProjectIdAndSiteId,
  getExpenseDetailsByExpenseId,
  updateStatus,
  getByExpenseCode,
  addIndependentExpense,
  updateIndependentExpense,
};
