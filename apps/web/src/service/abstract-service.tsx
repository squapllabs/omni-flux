import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const createAbstractData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/abstract/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in abstract insert :', error);
    throw error;
  }
};


export default {
    createAbstractData,
};
