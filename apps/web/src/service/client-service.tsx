import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllClient = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/client/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all client:', error);
    throw error;
  }
};

const getOneClientByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/client/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne uom :', error);
    throw error;
  }
};

const createClient = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/client/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in client create :', error);
    throw error;
  }
};
const updateClient = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/client/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in client edit:', error);
  }
};
const deleteClient = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/client/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete client list :', error);
    throw error;
  }
};
const filterClient = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/client/search-client`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in client search :', error);
    throw error;
  }
};
export default {
  getAllClient,
  getOneClientByID,
  createClient,
  updateClient,
  deleteClient,
  filterClient,
};
