import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllSubSubcategory = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-sub-category/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all sub-sub-category:', error);
    throw error;
  }
};

const getOneSubSubcategoryByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-sub-category/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne sub-sub-category :', error);
    throw error;
  }
};

const createSubSubcategory = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/sub-sub-category/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in sub-sub-category create :', error);
    throw error;
  }
};
const updateSubSubcategory = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/sub-sub-category/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in sub-sub-categoryy edit :', error);
  }
};
const deleteSubSubcategory = async (id: number) => {
  console.log('id in delete service', id);

  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/sub-sub-category/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete sub-sub-category :', error);
    throw error;
  }
};

const checkDublicateSubSubCategory = async (value: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-sub-category/checkDuplicateName/${value.name}/${value.id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in checkDublicateSubSubCategory   :', error);
    throw error;
  }
};

const filterSubSubCategory = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/sub-sub-category/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in sub-sub-category filter :', error);
    throw error;
  }
};

const getOneSubSubCatListbySubCatID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-sub-category/get-by-sub-category-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get sub sub category  list:', error);
    throw error;
  }
};
const getChildbyParentID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/sub-sub-category/get-child-data-by-parent-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get sub sub category  list:', error);
    throw error;
  }
};

export default {
  getAllSubSubcategory,
  getOneSubSubcategoryByID,
  createSubSubcategory,
  updateSubSubcategory,
  deleteSubSubcategory,
  checkDublicateSubSubCategory,
  filterSubSubCategory,
  getOneSubSubCatListbySubCatID,
  getChildbyParentID,
};
