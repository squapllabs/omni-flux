import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAlluom = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/uom/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all uom:', error);
    throw error;
  }
};

const getOneUomByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/uom/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne uom :', error);
    throw error;
  }
};

const createUom = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/uom/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in uom create :', error);
    throw error;
  }
};
const updateUom = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/uom/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in uom edit:', error);
  }
};
const deleteUom = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/uom/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete uom list :', error);
    throw error;
  }
};

const getOneUomByName = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/uom/getByName/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne uom :', error);
    throw error;
  }
};
const getUomByType = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/uom/get-by-uom-type/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getUomByType uom :', error);
    throw error;
  }
};
const filterUom = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/uom/search-uom`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in uom search :', error);
    throw error;
  }
};

const getOneUomByType = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/uom/get-by-uom-type/LABOR`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOneUomaByType master-data :', error);
    throw error;
  }
};

export default {
  getAlluom,
  getOneUomByID,
  createUom,
  updateUom,
  deleteUom,
  getOneUomByName,
  filterUom,
  getOneUomByType,
  getUomByType,
};
