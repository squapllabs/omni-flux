import catchAsync from '../utils/catchAsync';
import * as fileUpload from '../utils/fileUpload';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const processFileUpload = catchAsync(async (req, res) => {
  const methodName = '/processFileUpload';
  try {
    const file = await fileUpload.processFileUpload(req);
    res.send(file);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const processFileDelete = catchAsync(async (req, res) => {
  const methodName = '/processFileDelete';
  try {
    const file = await fileUpload.processFileDeleteInS3(req.body);
    res.send(file);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { processFileUpload, processFileDelete };
