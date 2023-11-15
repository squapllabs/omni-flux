import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const addProduct = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/item/add-item`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in Add product  api :', error);
    throw error;
  }
};
const getAllItems = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/item/get-all`,
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all items', error);
    throw error;
  }
};

const deleteItem = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/item/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error while deleting an item :', error);
    throw error;
  }
};

const getOneByItemID =async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/item/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne product :', error);
    throw error;
  }
}

const updateItem = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/item/update-item`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in item edit:', error);
  }
};

const filterItem = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/item/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in item search :', error);
    throw error;
  }
}

const getOneItemByName = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/item/check-duplicate-name/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne uom :', error);
    throw error;
  }
};

const getOneItemByCode = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/item/check-duplicate-code/${values}`
    );
    // console.log("service",response.data);
    return response.data;
  } catch (error) {
    console.log('Error in getOneItemByCode:', error);
    throw error;
  }
};

export default {
  addProduct,
  getAllItems,
  deleteItem,
  getOneByItemID,
  updateItem,
  filterItem,
  getOneItemByName,
  getOneItemByCode
};
