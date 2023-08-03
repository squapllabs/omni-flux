import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllSiteDrop = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/site/getAll/`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting site values:', error);
    throw error;
  }
};

export default {
    getAllSiteDrop,
};
