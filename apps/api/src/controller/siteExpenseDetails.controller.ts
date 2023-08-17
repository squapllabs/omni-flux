import catchAsync from '../utils/catchAsync';
import * as siteExpenseDetailsService from '../services/siteExpenseDetails.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createSiteExpenseDetails = catchAsync(async (req, res) => {
  const methodName = '/createSiteExpenseDetails';
  try {
    const siteExpenseDetails =
      await siteExpenseDetailsService.createSiteExpenseDetails(req.body);
    res.send(siteExpenseDetails);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const addBulkSiteExpenseDetails = catchAsync(async (req, res) => {
  const methodName = '/addBulkSiteExpenseDetails';
  try {
    const siteExpenseDetails =
      await siteExpenseDetailsService.addBulkSiteExpenseDetails(req.body);
    res.send(siteExpenseDetails);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { createSiteExpenseDetails, addBulkSiteExpenseDetails };
