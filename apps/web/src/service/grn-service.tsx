import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const createGrnData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/grn/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in grn  create  :', error);
    throw error;
  }
};

const filterGrn = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/grn/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in grn filter :', error);
    throw error;
  }
};

const getGrnById = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/grn/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne grn :', error);
    throw error;
  }
};

const getGrnByPOId = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/grn/get-by-purchase-order-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne grn By PO :', error);
    throw error;
  }
};

export default {
  createGrnData,
  filterGrn,
  getGrnById,
  getGrnByPOId,
};
