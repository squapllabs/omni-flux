import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllItems = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/item/get-all`
    );

    return response.data;
  } catch (error) {
    console.log('Error in getting all items', error);
    throw error;
  }
};

export default {
  getAllItems,
};
