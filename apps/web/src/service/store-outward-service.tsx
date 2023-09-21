import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';


const getStockOutWardData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/stock-outward/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in getStockOutWardData :', error);
    throw error;
  }
};


export default {
    getStockOutWardData,
};
