import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllParentProjectBreakDown = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-workbreak-down/get-all-parent-data`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all master-data:', error);
    throw error;
  }
};

const createProjectBreakDownData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project-workbreak-down/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project work down break create  :', error);
    throw error;
  }
};

const filterProjectWorkBreakDownData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project-workbreak-down/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project workbreakdown filter :', error);
    throw error;
  }
};

const getOneProjectWorkBreakDownById = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-workbreak-down/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne project workbreak down :', error);
    throw error;
  }
};

const updateProjectBreakDownData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/project-workbreak-down/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in updateProjectBreakDownData :', error);
  }
};

const checkProjectBreakDownCodeDuplicate = async (values:any) => {
  const data = values.toUpperCase()
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-workbreak-down/check-duplicate-code/${data}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in check ProjectBreakDownData :', error);
  }
};
export default {
  getAllParentProjectBreakDown,
  createProjectBreakDownData,
  filterProjectWorkBreakDownData,
  getOneProjectWorkBreakDownById,
  updateProjectBreakDownData,
  checkProjectBreakDownCodeDuplicate
};
