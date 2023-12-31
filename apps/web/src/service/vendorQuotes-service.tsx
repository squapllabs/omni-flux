import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const vendorQuotesData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/vendor-quotes/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in vendor-quotes search :', error);
    throw error;
  }
};

const getOneVendorQuotesById = async (values: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor-quotes/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in vendor-quotes update :', error);
    throw error;
  }
};
const getVendorQuotesBasedONPR = async (values: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor-quotes/get-by-purchase-request-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in vendor-quotes  :', error);
    throw error;
  }
};
const getVendorDetailsBasedONPR = async (values: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor-quotes/get-vendor-details-by-purchase-request-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in vendor-quotes  :', error);
    throw error;
  }
};
const getVendorquotesBYventorquotesID = async (values: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/vendor-quotation-details/get-by-vendor-quotes-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log(
      'Error in vendor-quotes getVendorquotesBYventorquotesID :',
      error
    );
    throw error;
  }
};

const updateVendorQuotes = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/vendor-quotes/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in vendor-quotes edit:', error);
  }
};

const documentUpload = async (file: any, code: string, folder: string) => {
  const formData = new FormData();
  formData.append('storage', 's3');
  formData.append('file', file);
  formData.append('folder', folder);
  formData.append('code', code);
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/upload/file`,
      formData,
      {
        headers: {
          'Content-Type': 'multipart/form-data',
        },
      }
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur documentUpload :', error);
    throw error;
  }
};

export default {
  vendorQuotesData,
  getOneVendorQuotesById,
  updateVendorQuotes,
  documentUpload,
  getVendorQuotesBasedONPR,
  getVendorDetailsBasedONPR,
  getVendorquotesBYventorquotesID,
};
