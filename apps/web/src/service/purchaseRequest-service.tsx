import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const purchaseDetailData = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/purchase-request/search`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in purchaseDetailData search :', error);
      throw error;
    }
  };

  const getOneProjectRequestById = async (values: number) => {
    try {
      const response = await axiosinterceptor.get(
        `${environment.apiUrl}/purchase-request/get/${values}`
      );
      return response.data;
    } catch (error) {
      console.log('Error in purchase request update :', error);
      throw error;
    }
  };

  const updatePurchaseRequest = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.put(
        `${environment.apiUrl}/purchase-request/`,
        values
      );
      console.log("response.data",response.data);
      
      return response.data;
    } catch (error) {
      console.log('Error in purchase-request edit:', error);
    }
  };

  export default {
    purchaseDetailData,
    getOneProjectRequestById,
    updatePurchaseRequest
  };