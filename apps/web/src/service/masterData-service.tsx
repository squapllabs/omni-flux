import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllmasertData = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/master-data/get-all`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all master-data:', error);
    throw error;
  }
};
const getAllParentMasterData = async () => {
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

const getOnemasertDataByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/master-data/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne master-data :', error);
    throw error;
  }
};
const getOnemasertDataByType = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/master-data/get-by-type/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOnemasertDataByType master-data :', error);
    throw error;
  }
};

const getOneMasterDataByProjectId = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/master-data/get-by-project-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOneMasterDataByProjectId:', error);
    throw error;
  }
};

const createmasertData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/master-data/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in master-data create :', error);
    throw error;
  }
};
const updatemasertData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/master-data/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in master-data edit:', error);
  }
};
const deletemasertData = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/master-data/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete master-data  :', error);
    throw error;
  }
};

// const checkDublicatemasertData = async (value: any) => {
//   try {
//     let response;
//     if (value.id === null) {
//       response = await axiosinterceptor.get(
//         `${environment.apiUrl}/master-data/get-by-parent-type/${value.name}`
//       );
//     } else {
//       response = await axiosinterceptor.get(
//         `${environment.apiUrl}/master-data/get-by-parent-type/${value.name}/${value.id}`
//       );
//     }

//     return response.data;
//   } catch (error) {
//     console.log('Error in occur in checkDublicatemasertData   :', error);
//     throw error;
//   }
// };

const checkDublicatemasertData = async (value: any) => {
  const data = value?.name?.toUpperCase();
  const projectID = Number(value?.project_id);
  const parent_id = Number(value?.id);
  const body = {
    parent_master_data_id: parent_id,
    master_data_type: data,
    project_id: projectID,
  };
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/master-data/get-by-parent-type/`,
      body
    );
    return response.data;
  } catch (error) {
    console.log('Error in checkDublicatemasertData: ', error);
    throw error;
  }
};
const filtermasertData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/master-data/search-master-data`,
      values
    );

    return response.data;
  } catch (error) {
    console.log('Error in master-data filter :', error);
    throw error;
  }
};

const getAllCurrencyData = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/master-data/get-by-type/CRTYP`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting getAllCurrencyData:', error);
    throw error;
  }
};

const checkDublicateProjectMasertData = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/master-data/get-by-project-id-and-type/${values.id}/${values.type}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in checkDublicateProjectMasertData:', error);
    throw error;
  }
};

export default {
  getAllmasertData,
  getOnemasertDataByID,
  createmasertData,
  updatemasertData,
  deletemasertData,
  checkDublicatemasertData,
  filtermasertData,
  getAllParentMasterData,
  getAllCurrencyData,
  getOnemasertDataByType,
  checkDublicateProjectMasertData,
  getOneMasterDataByProjectId,
};
