import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const addProduct = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/api/product/createProduct`,
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
      `${environment.apiUrl}/api/item/getAllItem`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all items', error);
    throw error;
  }
};

export default {
  addProduct,
  getAllItems,
};
