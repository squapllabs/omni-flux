import catchAsync from '../utils/catchAsync';
import * as expenseDetailsService from '../services/expenseDetails.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const updateStatus = catchAsync(async (req, res) => {
  const methodName = '/updateStatus';
  try {
    const expenseDetails = await expenseDetailsService.updateStatus(req.body);
    res.send(expenseDetails);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getById = catchAsync(async (req, res) => {
  const methodName = '/getById';
  try {
    const expenseDetails = await expenseDetailsService.getById(
      req.params.expense_details_id
    );
    res.send(expenseDetails);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { updateStatus, getById };
