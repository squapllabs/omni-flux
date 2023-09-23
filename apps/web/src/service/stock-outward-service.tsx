import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';


const getStockOutWardData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/stock-outward/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in getStockOutWardData :', error);
    throw error;
  }
};

const getProjectInventoryItem = async (data: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-inventory/get-by-project-id/${data}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in getProjectInventoryItem :', error);
    throw error;
  }
};

const addStockOutWard = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/stock-outward/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in addStockOutWard :', error);
    throw error;
  }
};

const getOneStockOutWardId = async (id: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/stock-outward/get/${id}`
    );
    console.log("res",response.data);
    return response.data;    
  } catch (error) {
    console.log('Error in occur in getOneStockOutWardId :', error);
    throw error;
  }
}

const updateStockOutWard = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/stock-outward/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in updateStockOutWard :', error);
    throw error;
  }
};


export default {
  getStockOutWardData,
  getProjectInventoryItem,
  addStockOutWard,
  getOneStockOutWardId,
  updateStockOutWard
};
