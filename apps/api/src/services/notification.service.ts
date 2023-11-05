import notificationDao from '../dao/notification.dao';
import { notificationBody } from '../interfaces/notification.interface';

/**
 * Method to Create a New Notification
 * @param body
 * @returns
 */
const createNotification = async (body: notificationBody) => {
  try {
    const {
      notification_from_user_id,
      notification_to_user_id,
      notification_type_id,
      notification_type,
      notification_description,
      created_by,
    } = body;
    const notificationDetails = await notificationDao.add(
      notification_from_user_id,
      notification_to_user_id,
      notification_type_id,
      notification_type,
      notification_description,
      created_by
    );
    const result = {
      message: 'success',
      status: true,
      data: notificationDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in notification service Add: ', error);
    throw error;
  }
};

/**
 * Method to get Notification By NotificationId
 * @param notificationId
 * @returns
 */
const getById = async (notificationId: number) => {
  try {
    let result = null;
    const notificationData = await notificationDao.getById(notificationId);
    if (notificationData) {
      result = { message: 'success', status: true, data: notificationData };
      return result;
    } else {
      result = {
        message: 'notification_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById notification service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Notifications
 * @returns
 */
const getAllNotifications = async () => {
  try {
    const result = await notificationDao.getAll();
    const notificationData = { message: 'success', status: true, data: result };
    return notificationData;
  } catch (error) {
    console.log(
      'Error occurred in getAllNotifications notification service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search Notification - Pagination API
 * @returns
 */
const searchNotification = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const notification_to_user_id = body.notification_to_user_id;

    const filterObj: any = {};

    if (notification_to_user_id) {
      filterObj.filterNotification = {
        notification_to_user_id: notification_to_user_id,
      };
    }

    const result = await notificationDao.searchNotification(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );
    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    if (result.count >= 0) {
      const tempNotificationData = {
        message: 'success',
        status: true,
        total_count: count,
        total_page: total_pages,
        is_available: true,
        content: data,
      };
      return tempNotificationData;
    } else {
      const tempNotificationData = {
        message: 'No data found',
        status: false,
        is_available: false,
      };
      return tempNotificationData;
    }
  } catch (error) {
    console.log(
      'Error occurred in searchNotification Notification service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get Unread Count By To User Id
 * @param notification_to_user_id
 * @returns
 */
const getUnreadCountByToUserId = async (notification_to_user_id: number) => {
  try {
    let result = null;
    const notificationData = await notificationDao.getUnreadCountByToUserId(
      notification_to_user_id
    );
    if (notificationData) {
      result = { message: 'success', status: true, data: notificationData };
      return result;
    } else {
      result = {
        message: 'No data found for this user_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById notification service : ', error);
    throw error;
  }
};

/**
 * Method to Get Unread Count By To User Id
 * @param body
 * @returns
 */
const updateIsReadByToUserId = async (body) => {
  try {
    const { notification_to_user_id } = body;
    const notificationData = await notificationDao.updateIsReadByToUserId(
      Number(notification_to_user_id)
    );
    if (notificationData.length > 0) {
      return { message: 'success', status: true, data: notificationData };
    } else {
      return {
        message: 'No data found for this user_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log('Error occurred in getById notification service : ', error);
    throw error;
  }
};

export {
  createNotification,
  getAllNotifications,
  getById,
  searchNotification,
  getUnreadCountByToUserId,
  updateIsReadByToUserId,
};
