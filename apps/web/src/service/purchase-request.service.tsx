import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getOnePurchaseRequest = async (id: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/purchase-request/get/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get by id getOnePurchaseRequest:', error);
    throw error;
  }
};

export default {
    getOnePurchaseRequest
};
