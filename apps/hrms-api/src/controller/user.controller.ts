import catchAsync from '../utils/catchAsync';
import * as userService from '../services/user.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const getAllUser = catchAsync(async (req, res) => {
  const methodName = '/getAll';
  try {
    const users = await userService.getAllUser(req.params.user_status);
    res.send(users);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { getAllUser };
