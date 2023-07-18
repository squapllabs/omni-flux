import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllSubcategory = async () => {
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

const getOneSubcategoryByID = async (values: any) => {
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

const createSubcategory = async (values: JSON) => {
  console.log('values', values);

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
const updateSubcategory = async (values: JSON) => {
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
const deleteSubcategory = async (id: number) => {
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
export default {
  getAllSubcategory,
  getOneSubcategoryByID,
  createSubcategory,
  updateSubcategory,
  deleteSubcategory,
};
