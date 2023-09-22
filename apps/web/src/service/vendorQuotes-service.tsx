import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const vendorQuotesData = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/vendor-quotes/search`,
        values
      );
      // console.log("response.data",response.data);
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

  const updateVendorQuotes = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.put(
        `${environment.apiUrl}/vendor-quotes/`,
        values
      );
      console.log("response.data",response.data);
      
      return response.data;
    } catch (error) {
      console.log('Error in vendor-quotes edit:', error);
    }
  };

  const documentUpload = async (file: any, code: string,folder:string) => {
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



  export default {vendorQuotesData,getOneVendorQuotesById,updateVendorQuotes,documentUpload};