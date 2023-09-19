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

  export default {
    purchaseDetailData,
    getOneProjectRequestById
  };