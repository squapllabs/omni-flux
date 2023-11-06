import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';


const getOnePurchaseOrderById = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne getOnePurchaseOrderById :', error);
    throw error;
  }
};
export default {
    getOnePurchaseOrderById
};
