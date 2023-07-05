import catchAsync from '../utils/catchAsync';
import * as forgetpasswordService from '../services/auth.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';


const forgetPassword = catchAsync(async (req, res) => {
    const methodName = '/forgetPassword';
    try {
      const result = await forgetpasswordService.forgetPassword(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });

  export { forgetPassword};