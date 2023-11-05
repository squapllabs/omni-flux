import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const filterNotification = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/notification/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in notification :', error);
    throw error;
  }
};

const getNotificationCountByUserID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/notification/get-unread-count-by-to-user-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting notification count :', error);
    throw error;
  }
};

const updateNotificationStatusByUser = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/notification/mark-all-as-read-by-to-user-id`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in updating notification seen status:', error);
  }
};

export default {
  filterNotification,
  getNotificationCountByUserID,
  updateNotificationStatusByUser,
};
