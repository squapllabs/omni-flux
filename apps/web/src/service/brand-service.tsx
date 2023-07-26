import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllBrand = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/brand/get-all-brands`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all gst data:', error);
    throw error;
  }
};

export default {
  getAllBrand,
};
