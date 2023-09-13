import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const createBomData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/bom/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in bom insert :', error);
    throw error;
  }
};
const createBulkBom = async (values: any) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/bom/add-bulk`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in bom insert :', error);
    throw error;
  }
};
const getBOMbySubCatIDandType = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/bom/get-by-sub-cat-id-and-bom-type/${values.id}/${values.type}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in bom insert :', error);
    throw error;
  }
};
const getBOMbySubCatID = async (values: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/bom/get-by-sub-category-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in bom insert :', error);
    throw error;
  }
};

const getCustomBomData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/bom/get-by-category-combo`,
      values
    );
    console.log('response in service==>', response);

    return response.data;
  } catch (error) {
    console.log('Error in getCustomBomData  :', error);
    throw error;
  }
};

const getBOMbyProjectandType = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/bom/get-by-project-id-and-bom-type/${values.id}/${values.type}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in bom getBOMbyProjectandType :', error);
    throw error;
  }
};
export default {
  createBomData,
  getCustomBomData,
  createBulkBom,
  getBOMbySubCatIDandType,
  getBOMbySubCatID,
  getBOMbyProjectandType,
};
