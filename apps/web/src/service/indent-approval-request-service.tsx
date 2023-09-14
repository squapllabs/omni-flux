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

  export default {
    indentRaise
  };
  