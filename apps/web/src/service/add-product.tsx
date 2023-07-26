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
const getAllItems = async (values) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/item/get-all-items`,
      values
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

export default {
  addProduct,
  getAllItems,
  deleteItem,
};
