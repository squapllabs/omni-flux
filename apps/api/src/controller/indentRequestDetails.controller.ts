import catchAsync from '../utils/catchAsync';
import * as indentRequestDetailsService from '../services/indentRequestDetails.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const searchIndentRequestDetails = catchAsync(async (req, res) => {
  const methodName = '/searchIndentRequestDetails';
  try {
    const indentRequestDetails =
      await indentRequestDetailsService.searchIndentRequestDetails(req.body);
    res.send(indentRequestDetails);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { searchIndentRequestDetails };
