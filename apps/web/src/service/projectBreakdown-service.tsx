import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllParentProjectBreakDown = async () => {
    try {
      const response = await axiosinterceptor.get(
        `${environment.apiUrl}/master-data/get-all-parent-master-data`
      );
      return response.data;
    } catch (error) {
      console.log('Error in getting all master-data:', error);
      throw error;
    }
  };

  const createProjectBreakDownData = async (values: JSON) => {
    console.log("data ==>",values)
    try {   
      const response = await axiosinterceptor.post(
        `${environment.apiUrl}/project-workbreak-down/`,
        values
      );
      return response.data;
    } catch (error) {
      console.log('Error in project work down break create  :', error);
      throw error;
    }
  };

  export default {
    getAllParentProjectBreakDown,
    createProjectBreakDownData
  };
  