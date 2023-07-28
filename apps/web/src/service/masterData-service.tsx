import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllmasertData = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-category/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all category:', error);
    throw error;
  }
};

const getOnemasertDataByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-category/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne category :', error);
    throw error;
  }
};

const createmasertData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/sub-category/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in sub-category create :', error);
    throw error;
  }
};
const updatemasertData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/sub-category/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in sub-category edit:', error);
  }
};
const deletemasertData = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/sub-category/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete sub-category  :', error);
    throw error;
  }
};

const checkDublicatemasertData = async (value: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-category/checkDuplicateName/${value.name}/${value.id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in checkDublicatemasertData   :', error);
    throw error;
  }
};
const filtermasertData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/sub-category/search`,
      values
    );
    console.log('response.data', response.data);

    return response.data;
  } catch (error) {
    console.log('Error in sub-category filter :', error);
    throw error;
  }
};

export default {
  getAllmasertData,
  getOnemasertDataByID,
  createmasertData,
  updatemasertData,
  deletemasertData,
  checkDublicatemasertData,
  filtermasertData,
};
