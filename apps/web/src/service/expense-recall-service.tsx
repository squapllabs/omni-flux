import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const createSiteExpenseRecall = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/expense-recall/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in expense recall create :', error);
    throw error;
  }
};

export default { createSiteExpenseRecall };
