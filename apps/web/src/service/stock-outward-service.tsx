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

const getProjectInventoryItem = async (data: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/user/get-users-by-role-name/${data?.role}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in getProjectInventoryItem :', error);
    throw error;
  }
};



export default {
    getStockOutWardData,
    getProjectInventoryItem
};
