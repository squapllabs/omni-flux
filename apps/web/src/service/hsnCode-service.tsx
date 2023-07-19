import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllHsnCode = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/hsn-code/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all hsn code data:', error);
    throw error;
  }
};

const deleteHsnCode = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/hsn-code/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete hsc code :', error);
    throw error;
  }
};

const createHsnCode = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/hsn-code/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in post gst api :', error);
    throw error;
  }
};

const getOneHsnCode = async (id: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/hsn-code/get/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get by id hsn code data:', error);
    throw error;
  }
};

const updateHsnCode = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/hsn-code/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in update hsc-code api :', error);
  }
};

export default {
  getAllHsnCode,
  deleteHsnCode,
  createHsnCode,
  getOneHsnCode,
  updateHsnCode,
};
