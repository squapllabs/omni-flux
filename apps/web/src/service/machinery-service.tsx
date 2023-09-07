import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllMachinery = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/machinery/get-all`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all machinery:', error);
    throw error;
  }
};

export default {
  getAllMachinery,
};
