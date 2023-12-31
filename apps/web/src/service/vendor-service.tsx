import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const filterVendor = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/vendor/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in client search :', error);
    throw error;
  }
};

const deleteVendor = async (id: any) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/vendor/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete vendor list :', error);
    throw error;
  }
};

const createVendor = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/vendor/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in vendor create :', error);
    throw error;
  }
};

const getOneVendorById = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne vendor data :', error);
    throw error;
  }
};

const updateVendors = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/vendor/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in updateVendors edit:', error);
  }
};

const getOneVendorEmail = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor/get-by-email-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOneVendorEmail :', error);
    throw error;
  }
};

const getAllVendors = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor/get-all`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all Vendors:', error);
    throw error;
  }
};

const getByVendorName = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor/check-duplicate-name/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getByVendorName:', error);
    throw error;
  }
};

const getByVendorCode = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor/check-duplicate-code/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getByVendorCode:', error);
    throw error;
  }
};

export default {
  filterVendor,
  deleteVendor,
  createVendor,
  getOneVendorById,
  updateVendors,
  getOneVendorEmail,
  getAllVendors,
  getByVendorName,
  getByVendorCode
}