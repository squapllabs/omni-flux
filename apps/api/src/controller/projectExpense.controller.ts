import catchAsync from '../utils/catchAsync';
import * as projectExpenseService from '../services/projectExpense.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createProjectExpense = catchAsync(async (req, res) => {
  const methodName = '/createProjectExpense';
  try {
    const projectExpense = await projectExpenseService.createProjectExpense(
      req.body
    );
    res.send(projectExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateProjectExpense = catchAsync(async (req, res) => {
  const methodName = '/updateProjectExpense';
  try {
    const projectExpense = await projectExpenseService.updateProjectExpense(
      req.body
    );
    res.send(projectExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllProjectExpense = catchAsync(async (req, res) => {
  const methodName = '/getAllProjectExpense';
  try {
    const projectExpense = await projectExpenseService.getAllProjectExpense();
    res.send(projectExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectExpenseId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectExpenseId';
  try {
    const projectExpense = await projectExpenseService.getById(
      req.params.project_expense_id
    );
    res.send(projectExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByPojectExpenseId = catchAsync(async (req, res) => {
  const methodName = '/deleteByPojectExpenseId';
  try {
    const projectExpense = await projectExpenseService.deleteProjectExpense(
      req.params.project_expense_id
    );
    res.send(projectExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createProjectExpense,
  updateProjectExpense,
  getAllProjectExpense,
  getByProjectExpenseId,
  deleteByPojectExpenseId,
};
