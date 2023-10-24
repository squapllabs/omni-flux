import catchAsync from '../utils/catchAsync';
import * as expenseRecallService from '../services/expenseRecall.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createExpenseRecall = catchAsync(async (req, res) => {
  const methodName = '/createExpenseRecall';
  try {
    const expenseRecall = await expenseRecallService.createExpenseRecall(
      req.body
    );
    res.send(expenseRecall);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { createExpenseRecall };
