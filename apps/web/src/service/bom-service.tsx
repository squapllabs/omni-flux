import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const createBomData = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/bom/`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in bom insert :', error);
      throw error;
    }
  };

  export default {
    createBomData
  }