import catchAsync from '../utils/catchAsync';
import * as siteExpenseService from '../services/siteExpense.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createSiteExpense = catchAsync(async (req, res) => {
  const methodName = '/createSiteExpense';
  try {
    const siteExpense = await siteExpenseService.createSiteExpense(req.body);
    res.send(siteExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateSiteExpense = catchAsync(async (req, res) => {
  const methodName = '/updateSiteExpense';
  try {
    const siteExpense = await siteExpenseService.updateSiteExpense(req.body);
    res.send(siteExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllSiteExpense = catchAsync(async (req, res) => {
  const methodName = '/getAllSiteExpense';
  try {
    const siteExpense = await siteExpenseService.getAllSiteExpense();
    res.send(siteExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBySiteExpenseId = catchAsync(async (req, res) => {
  const methodName = '/getBySiteExpenseId';
  try {
    const siteExpense = await siteExpenseService.getById(
      req.params.site_expense_id
    );
    res.send(siteExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteBySiteExpenseId = catchAsync(async (req, res) => {
  const methodName = '/deleteBySiteExpenseId';
  try {
    const siteExpense = await siteExpenseService.deleteSiteExpense(
      req.params.site_expense_id
    );
    res.send(siteExpense);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createSiteExpense,
  updateSiteExpense,
  getAllSiteExpense,
  getBySiteExpenseId,
  deleteBySiteExpenseId,
};
