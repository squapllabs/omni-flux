import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getIndentByProjectID = async (values: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/indent-request/get-by-project-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in Indent insert :', error);
    throw error;
  }
};
const getOneIndent = async (values: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/indent-request/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get one Indent  :', error);
    throw error;
  }
};

const createIndentRequest = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/indent-request/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in createIndentRequest  api :', error);
    throw error;
  }
};
const updateIndentRequest = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/indent-request/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in createIndentRequest  api :', error);
    throw error;
  }
};

export default {
  getIndentByProjectID,
  createIndentRequest,
  getOneIndent,
  updateIndentRequest,
};
