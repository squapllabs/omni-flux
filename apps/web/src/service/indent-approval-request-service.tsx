import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const indentRaise = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/indent-request/search`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in indentRaise search :', error);
      throw error;
    }
  };

  const indentDetailData = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/indent-request-details/search`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in indentDetailData search :', error);
      throw error;
    }
  };

  
  const updateIndentRequest = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.put(
        `${environment.apiUrl}/indent-request/update-status`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in updateIndentRequest :', error);
      throw error;
    }
  };

  const getOneIndentById = async (values: any) => {
    try {
      const response = await axiosinterceptor.get(
        `${environment.apiUrl}/indent-request/get/${values}`
      );
      return response.data;
    } catch (error) {
      console.log('Error in getOne indent request :', error);
      throw error;
    }
  };

  export default {
    indentRaise,
    indentDetailData,
    updateIndentRequest,
    getOneIndentById
  };
  