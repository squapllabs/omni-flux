import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllLabours = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/labour/get-all`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all labour:', error);
    throw error;
  }
};

const filterLabour = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/labour/search`,
      values
    );
    console.log("response.data =====>/////",response.data);
    
    return response.data;
  } catch (error) {
    console.log('Error in labour filter :', error);
    throw error;
  }
};

const getLaboursUom = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/uom/get-by-uom-type/LABOR`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting LaboursUom:', error);
    throw error;
  }
};

const addLabour = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/labour/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in labour create :', error);
    throw error;
  }
};

const getOneLabourID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/labour/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne Labour :', error);
    throw error;
  }
};

const editLabour = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/labour/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in labour edit:', error);
  }
};

const deleteLabour = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/labour/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete labour  :', error);
    throw error;
  }
};

const getByLabourType = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/labour/check-duplicate-labour-type/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne Labour Type :', error);
    throw error;
  }
};

export default {
  getAllLabours,
  filterLabour,
  getLaboursUom,
  addLabour,
  getOneLabourID,
  editLabour,
  deleteLabour,
  getByLabourType
};
