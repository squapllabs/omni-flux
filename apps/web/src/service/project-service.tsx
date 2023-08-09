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

const createProjectData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project  create  :', error);
    throw error;
  }
};

const checkProjectCodeDuplicate = async (values:any) => {
  const data = values.toUpperCase()
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project/check-duplicate-code/${data}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in check checkSiteCodeDuplicate :', error);
  }
};

export default {
  getAllProject,
  createProjectData,
  checkProjectCodeDuplicate
};
