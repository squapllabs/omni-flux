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

const checkDublicateSubCategory = async (value: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-category/checkDuplicateName/${value.name}/${value.id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in checkDublicateSubCategory   :', error);
    throw error;
  }
};
const filterSubCategory = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/sub-category/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in sub-category filter :', error);
    throw error;
  }
};

const getOneSubCatListbyCatID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-category/get-by-category-id/${values.selectedCategory}/${values.selectedBomConfig}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get sub category  list', error);
    throw error;
  }
};
const getOneChlidSubCatListbyParentID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-category/get-by-parent-sub-category-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get getOneChlidSubCatListbyParentID  list', error);
    throw error;
  }
};

export default {
  getAllSubcategory,
  getOneSubcategoryByID,
  createSubcategory,
  updateSubcategory,
  deleteSubcategory,
  checkDublicateSubCategory,
  filterSubCategory,
  getOneSubCatListbyCatID,
  getOneChlidSubCatListbyParentID,
};
