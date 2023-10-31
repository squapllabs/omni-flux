import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const createGrnData = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/grn/`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in grn  create  :', error);
      throw error;
    }
  };

  export default {
    createGrnData
  }