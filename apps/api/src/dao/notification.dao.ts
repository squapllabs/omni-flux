import prisma from '../utils/prisma';
import db from '../utils/db';

const add = async (
  notification_from_user_id: number,
  notification_to_user_id: number,
  notification_type_id: number,
  notification_type: string,
  notification_description: string,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_read = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const notification = await transaction.notifications.create({
      data: {
        notification_from_user_id,
        notification_to_user_id,
        notification_type_id,
        notification_type,
        notification_description,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_read: is_read,
      },
    });
    return notification;
  } catch (error) {
    console.log('Error occurred in notificationDao add', error);
    throw error;
  }
};

const getById = async (notificationId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const notification = await transaction.notifications.findFirst({
      where: {
        notification_id: Number(notificationId),
      },
    });
    return notification;
  } catch (error) {
    console.log('Error occurred in notification getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const notification = await transaction.notifications.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return notification;
  } catch (error) {
    console.log('Error occurred in notification getAll dao', error);
    throw error;
  }
};

const searchNotification = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterNotification;
    const notification = await transaction.notifications.findMany({
      where: filter,
      include: {
        notification_from_user_data: {
          select: {
            user_id: true,
            first_name: true,
            last_name: true,
            user_profiles: {
              select: {
                profile_image_url: true,
              },
            },
          },
        },
        notification_to_user_data: {
          select: {
            user_id: true,
            first_name: true,
            last_name: true,
            user_profiles: {
              select: {
                profile_image_url: true,
              },
            },
          },
        },
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const notificationCount = await transaction.notifications.count({
      where: filter,
    });
    const notificationData = {
      count: notificationCount,
      data: notification,
    };
    return notificationData;
  } catch (error) {
    console.log(
      'Error occurred in notification dao : searchNotification',
      error
    );
    throw error;
  }
};

const getUnreadCountByToUserId = async (
  notification_to_user_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const notification = await transaction.notifications.count({
      where: {
        notification_to_user_id: Number(notification_to_user_id),
        is_read: false,
      },
    });
    return notification;
  } catch (error) {
    console.log(
      'Error occurred in notification getUnreadCountByToUserId dao',
      error
    );
    throw error;
  }
};

const updateIsReadByToUserId = async (
  notification_to_user_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const currentDate = new Date();
    const query = `update notifications set is_read =true,updated_date=$2 where notification_to_user_id=$1 returning *`;
    const notification = await transaction.manyOrNone(query, [
      notification_to_user_id,
      currentDate,
    ]);
    return notification;
  } catch (error) {
    console.log(
      'Error occurred in notification updateIsReadByToUserId dao',
      error
    );
    throw error;
  }
};

export default {
  add,
  getById,
  getAll,
  searchNotification,
  getUnreadCountByToUserId,
  updateIsReadByToUserId,
};
