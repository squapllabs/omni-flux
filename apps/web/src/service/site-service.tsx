import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllSiteDrop = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/site-contractor/get-all-sites/`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting site values:', error);
    throw error;
  }
};

const createNewSite = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/site-contractor/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in createNewSite  :', error);
    throw error;
  }
};

export default {
    getAllSiteDrop,
    createNewSite
};
