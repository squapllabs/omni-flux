import catchAsync from '../utils/catchAsync';
import * as notificationService from '../services/notification.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createNotification = catchAsync(async (req, res) => {
  const methodName = '/createNotification';
  try {
    const notification = await notificationService.createNotification(req.body);
    res.send(notification);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllNotifications = catchAsync(async (req, res) => {
  const methodName = '/getAllNotifications';
  try {
    const notification = await notificationService.getAllNotifications();
    res.send(notification);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByNotificationId = catchAsync(async (req, res) => {
  const methodName = '/getByNotificationId';
  try {
    const notification = await notificationService.getById(
      req.params.notification_id
    );
    res.send(notification);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchNotification = catchAsync(async (req, res) => {
  const methodName = '/searchNotification';
  try {
    const notification = await notificationService.searchNotification(req.body);
    res.send(notification);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getUnreadCountByToUserId = catchAsync(async (req, res) => {
  const methodName = '/getUnreadCountByToUserId';
  try {
    const notification = await notificationService.getUnreadCountByToUserId(
      req.params.notification_to_user_id
    );
    res.send(notification);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateIsReadByToUserId = catchAsync(async (req, res) => {
  const methodName = '/updateIsReadByToUserId';
  try {
    const notification = await notificationService.updateIsReadByToUserId(
      req.body
    );
    res.send(notification);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createNotification,
  getAllNotifications,
  getByNotificationId,
  searchNotification,
  getUnreadCountByToUserId,
  updateIsReadByToUserId,
};
