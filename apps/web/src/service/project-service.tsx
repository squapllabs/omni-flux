import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllProject = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all project data:', error);
    throw error;
  }
};

export default {
  getAllProject,
};
