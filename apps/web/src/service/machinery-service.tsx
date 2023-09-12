import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllMachinery = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/machinery/get-all`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all Machinery:', error);
    throw error;
  }
};

const createMachinery = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/machinery/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in Machinery create :', error);
    throw error;
  }
};

const filterMachinery = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/machinery/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in machinery search :', error);
    throw error;
  }
};

const getOneMachineryByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/machinery/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne machinery :', error);
    throw error;
  }
};

const updateMachinery = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/machinery/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in Machinery edit:', error);
  }
};

const deleteMachinery = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/machinery/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete machinery list :', error);
    throw error;
  }
};

export default {getAllMachinery,createMachinery,filterMachinery,getOneMachineryByID,updateMachinery,deleteMachinery};
// export default {
//   getAllMachinery,
// };
