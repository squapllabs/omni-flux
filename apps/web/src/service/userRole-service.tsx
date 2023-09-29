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

const getAllSelectedRoles = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/role/getAll`
    );
    const filteredData = response?.data?.data?.filter(
      (role: any) =>
        role.role_name !== 'Admin' && role.role_name !== 'Project Manager'
    );
    return filteredData;
  } catch (error) {
    console.log('Error in getting getAllSelectedRoles:', error);
    throw error;
  }
};

export default { getAllRoles, getAllSelectedRoles };
