import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllCategory = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/category/getAll`
    );
    console.log('response', response);

    return response.data;
  } catch (error) {
    console.log('Error in getting all category:', error);
    throw error;
  }
};

const getOneCategoryByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/category/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne category :', error);
    throw error;
  }
};

const createCategory = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/category/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in category create :', error);
    throw error;
  }
};
const updateCategory = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/category/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in category edit:', error);
  }
};
const deleteCategory = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/category/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete category  :', error);
    throw error;
  }
};

const checkDublicateCategory = async (value: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/category/checkDuplicateName/${value.name}/${value.id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in checkDublicateCategory   :', error);
    throw error;
  }
};
export default {
  getAllCategory,
  getOneCategoryByID,
  createCategory,
  updateCategory,
  deleteCategory,
  checkDublicateCategory,
};
