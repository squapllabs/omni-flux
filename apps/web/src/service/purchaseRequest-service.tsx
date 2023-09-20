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

  const getProjectItems = async (values: number) => {
    try {
      const response = await axiosinterceptor.get(
        `${environment.apiUrl}/bom/get-all-items-by-project-id/${values}`
      );
      return response.data;
    } catch (error) {
      console.log('Error in get Project Items :', error);
      throw error;
    }
  };

  const addPurchaseRequest = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/purchase-request/`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in Add Purchase Request :', error);
      throw error;
    }
  };

  export default {
    purchaseDetailData,
    getOneProjectRequestById,
    getProjectItems,
    addPurchaseRequest
  };