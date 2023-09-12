import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllStore = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/category/get-all`
    );
    console.log('response.data', response.data);

    return response.data;
  } catch (error) {
    console.log('Error in getting all store:', error);
    throw error;
  }
};

const createStore = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/store/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in store create :', error);
    throw error;
  }
};

const updateStore = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/store/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in store edit:', error);
  }
};

const filterStore = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/store/search`,
      values
    );
    console.log("values",values);
    
    console.log("response.data",response.data);
    
    return response.data;
  } catch (error) {
    console.log('Error in store filter :', error);
    throw error;
  }
};

const deleteStore = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/store/delete/${id}`
    );
    console.log("delete",response.data);
    
    return response.data;
  } catch (error) {
    console.log('Error occured in delete store  :', error);
    throw error;
  }
};

const getOneStoreByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/store/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne store :', error);
    throw error;
  }
};

export default { createStore, getAllStore, updateStore ,filterStore,deleteStore,getOneStoreByID};
