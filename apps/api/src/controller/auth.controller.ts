import catchAsync from '../utils/catchAsync';
import * as forgetpasswordService from '../services/auth.service';
import * as userService from '../services/user.service';
import { handleError, ErrorHandler } from '../config/error';
import { setCookie } from './../utils/helper'

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

const editPassword = catchAsync(async (req, res) => {
  const methodName = '/edit';
  try {
    const result = await forgetpasswordService.updatePassword(req.body);
    res.send(result);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const logOut = catchAsync(async (req, res) => {
  const methodName = '/userLogOut';
  try {
    res.clearCookie('accessToken', {
      domain: process.env.NODE_ENV === 'LOCAL' ? 'localhost' : process.env.COOKIE_DOMAIN,
      httpOnly: false,
      secure: process.env.NODE_ENV === 'LOCAL' ? false : true,
      sameSite: process.env.NODE_ENV === 'LOCAL' ? 'strict' : 'none'
    });

    res.clearCookie('email', {
      domain: process.env.NODE_ENV === 'LOCAL' ? 'localhost' : process.env.COOKIE_DOMAIN,
      httpOnly: false,
      secure: process.env.NODE_ENV === 'LOCAL' ? false : true,
      sameSite: process.env.NODE_ENV === 'LOCAL' ? 'strict' : 'none'
    });
    res.clearCookie('refreshToken', {
      domain: process.env.NODE_ENV === 'LOCAL' ? 'localhost' : process.env.COOKIE_DOMAIN,
      httpOnly: false,
      secure: process.env.NODE_ENV === 'LOCAL' ? false : true,
      sameSite: process.env.NODE_ENV === 'LOCAL' ? 'strict' : 'none'
    });

    res.clearCookie('isRememberMe', {
      domain: process.env.NODE_ENV === 'LOCAL' ? 'localhost' : process.env.COOKIE_DOMAIN,
      httpOnly: false,
      secure: process.env.NODE_ENV === 'LOCAL' ? false : true,
      sameSite: process.env.NODE_ENV === 'LOCAL' ? 'strict' : 'none'
    });

    res.status(200).json({ status: true, message: 'LogOut successful', isLoggedIn: false });
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
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


const refreshToken = catchAsync(async (req, res) => {
  const refreshToken = req.body.refreshToken;

  if (!refreshToken) {
    return res
      .status(401)
      .json({ error: 'Unauthorized: No refresh token provided', accessToken: null, refreshToken: null });
  }

  try {
    const newAccessToken = await userService.refreshAccessToken(refreshToken);
    res.cookie('accessToken', newAccessToken, {
      httpOnly: false,
      maxAge: 365 * 24 * 60 * 60,
      sameSite: process.env.NODE_ENV === 'LOCAL' ? 'strict' : 'none',
      secure: process.env.NODE_ENV === 'LOCAL' ? false : true,
      domain:
        process.env.NODE_ENV === 'LOCAL'
          ? 'localhost'
          : process.env.COOKIE_DOMAIN,
    });
    res.json({ accessToken: newAccessToken, refreshToken });
  } catch (error) {
    res.status(401).json({ error: 'Unauthorized: Invalid refresh token', accessToken: null, refreshToken: null });
  }
});

const login = catchAsync(async (req, res) => {
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



export { forgetPassword, editPassword, loginValidate, login, refreshToken, isLoggedIn, logOut };