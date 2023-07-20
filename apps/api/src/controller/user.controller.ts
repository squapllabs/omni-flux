import catchAsync from '../utils/catchAsync';
import * as userService from '../services/user.service';
import { handleError, ErrorHandler } from '../config/error';
import { setCookie } from './../utils/helper'

const errorText = 'Error';

const createUser = catchAsync(async (req, res) => {
  const methodName = '/createUser';
  try {
    const result = await userService.createUser(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateUser = catchAsync(async (req, res) => {
  const methodName = '/updateUser';
  try {
    const result = await userService.updateUser(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByUserId = catchAsync(async (req, res) => {
  const methodName = '/getById';
  try {
    const result = await userService.getById(req.params.user_id);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByEmailId = catchAsync(async (req, res) => {
  const methodName = '/getByEmailId';
  try {
    const result = await userService.getByEmailId(req.params.email_id);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const userLogin = catchAsync(async (req, res) => {
  const methodName = '/userLogin';
  try {
    const result = await userService.userLogin(req.body, res);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const loginValidate = catchAsync(async (req, res) => {
  const methodName = '/loginValidate';
  try {

    const accessToken = req.body.accessToken;
    const email = req.body.email_id;
    const refreshToken = req.body.refreshToken;
    const isRememberMe = req.body.isRememberMe;


    Promise.all([
      setCookie(res, 'accessToken', accessToken, isRememberMe),
      setCookie(res, 'email', email, isRememberMe),
      setCookie(res, 'refreshToken', refreshToken, isRememberMe),
      setCookie(res, 'isRememberMe', isRememberMe, isRememberMe)
    ]).then(() => {
      // All cookies have been set
      res.setHeader('Cache-Control', 'no-store, no-cache, must-revalidate'); res.setHeader('Expires', '0');
      return res.send({ message: 'success', success: true });
    }).catch((error) => {
      // Handle any errors that occur while setting cookies
      console.error(error);
    });

  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});


const refreshToken = catchAsync(async (req, res) => {
  const refreshToken = req.body.refreshToken;

  if (!refreshToken) {
    return res
      .status(401)
      .json({ error: 'Unauthorized: No refresh token provided' });
  }

  try {
    const newAccessToken = await userService.refreshAccessToken(refreshToken);
    res.cookie('access_token', newAccessToken, {
      httpOnly: false,
      maxAge: 365 * 24 * 60 * 60,
      sameSite: process.env.NODE_ENV === 'LOCAL' ? 'strict' : 'none',
      secure: process.env.NODE_ENV === 'LOCAL' ? false : true,
      domain:
        process.env.NODE_ENV === 'LOCAL'
          ? 'localhost'
          : process.env.COOKIE_DOMAIN,
    });
    res.json({ access_token: newAccessToken });
  } catch (error) {
    res.status(401).json({ error: 'Unauthorized: Invalid refresh token' });
  }
});


const isLoggedIn = catchAsync(async (req, res) => {
  const methodName = '/isLoggedIn';
  try {

    const accessToken = req.cookies.token;
    const email = req.cookies.email_id;
    const refreshToken = req.cookies.refreshToken;
    const isRememberMe = req.cookies.isRememberMe;
    console.log("check console data-->", req)
    if (accessToken) {
      res.status(200).json({ isLoggedIn: true, accessToken, email, name, refreshToken, isRememberMe });
    } else {
      res.status(200).json({ isLoggedIn: false });
    }
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});




const getAllUser = catchAsync(async (req, res) => {
  const methodName = '/getAll';
  try {
    const users = await userService.getAllUser(req.params.user_status);
    res.send(users);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const userLogOut = catchAsync(async (req, res) => {
  const methodName = '/userLogOut';
  try {
    const result = await userService.userLogOut(req, res);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteUser = catchAsync(async (req, res) => {
  const methodName = '/deleteUser';
  try {
    const result = await userService.deleteUser(req.params.user_id);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateStatus = catchAsync(async (req, res) => {
  const methodName = '/updateStatus';
  try {
    const result = await userService.updateStatus(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchUser = catchAsync(async (req, res) => {
  const methodName = '/searchUser';
  try {
    const result = await userService.searchUser(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getDeletedUsers = catchAsync(async (req, res) => {
  const methodName = '/getDeletedUsers';
  try {
    const result = await userService.getDeletedUsers();
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createUser,
  updateUser,
  getByUserId,
  getByEmailId,
  userLogin,
  getAllUser,
  userLogOut,
  deleteUser,
  updateStatus,
  searchUser,
  getDeletedUsers,
  isLoggedIn,
  loginValidate,
  refreshToken
};
