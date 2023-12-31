import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllItemsType = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/itemType/get-all-item-types`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all items', error);
    throw error;
  }
};

export default {
  getAllItemsType,
};
