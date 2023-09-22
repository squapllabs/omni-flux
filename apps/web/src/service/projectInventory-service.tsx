import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const inventoryDetailData = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/project-inventory/search`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in indentDetailData search :', error);
      throw error;
    }
  };

  export default {inventoryDetailData};