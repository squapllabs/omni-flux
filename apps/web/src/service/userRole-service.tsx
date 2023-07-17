import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';
const getAllRoles = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/role/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all users:', error);
    throw error;
  }
};

export default { getAllRoles };
