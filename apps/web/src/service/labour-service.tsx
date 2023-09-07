import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllLabours = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/labour/get-all`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all category:', error);
    throw error;
  }
};

export default {
  getAllLabours,
};
