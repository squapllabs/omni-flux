import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllprojectSite = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-site/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all project-site:', error);
    throw error;
  }
};

const getOneprojectSiteByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-site/get-by-project-site-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne uom :', error);
    throw error;
  }
};

const createprojectSite = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project-site/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project-site create :', error);
    throw error;
  }
};
const updateprojectSite = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/project-site/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project-site edit:', error);
  }
};
const deleteprojectSite = async (id: any) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/project-site/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete project-site list :', error);
    throw error;
  }
};
const filterprojectSite = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project-site/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project-site search :', error);
    throw error;
  }
};

const getOneprojectSiteByName = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-site/check-duplicate-name/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne project-site name  :', error);
    throw error;
  }
};

export default {
  getAllprojectSite,
  getOneprojectSiteByID,
  createprojectSite,
  updateprojectSite,
  deleteprojectSite,
  filterprojectSite,
  getOneprojectSiteByName,
};
