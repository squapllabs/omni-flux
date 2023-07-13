import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllGst = async () => {
    try {
      const response = await axiosinterceptor.get(
        `${environment.apiUrl}/gst/getAll`
      );
      console.log("response===>",response)
      return response.data;
    } catch (error) {
      console.log('Error in getting all gst data:', error);
      throw error;
    }
  };

  const getOneGst = async (id : number) => {
    try {
      const response = await axiosinterceptor.get(
        `${environment.apiUrl}/gst/get/${id}`
      );
      return response.data;
    } catch (error) {
      console.log('Error in get by id gst data:', error);
      throw error;
    }
  };

  const deleteGst = async (id: number) => {
    try {
      const response = await axiosinterceptor.delete(
        `${environment.apiUrl}/gst/delete/${id}`
      );
      return response.data;
    } catch (error) {
      console.log('Error in occur in delete gst list :', error);
      throw error;
    }
  };

  const createGst = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/gst/`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in post gst api :', error);
      throw error;
    }
  };

  const updateGst = async (values: JSON) => {
    try {
      const response = await axiosinterceptor.put(
        `${environment.apiUrl}/gst/`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in update gst api :', error);
    }
  };

  export default {
    getAllGst,
    deleteGst,
    createGst,
    updateGst,
    getOneGst
  };
  