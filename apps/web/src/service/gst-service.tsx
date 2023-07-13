import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllGst = async () => {
    try {
      const response = await axiosinterceptor.get(
        `${environment.apiUrl}/gst/getAll`
      );
      return response.data;
    } catch (error) {
      console.log('Error in getting all gst data:', error);
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

  export default {
    getAllGst,
    deleteGst
  };
  