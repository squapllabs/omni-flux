import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const vendorQotesData = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/vendor-quotes/search`,
        values
      );
      console.log("response.data",response.data);
      
      return response.data;
    } catch (error) {
      console.log('Error in purchaseDetailData search :', error);
      throw error;
    }
  };



  export default {vendorQotesData};