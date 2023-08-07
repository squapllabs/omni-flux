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

const filterSiteData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/site-contractor/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project workbreakdown filter :', error);
    throw error;
  }
};

const getOneSiteById = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/site-contractor/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne site and contractor :', error);
    throw error;
  }
};

const updateSiteData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/site-contractor/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in updateSiteData :', error);
  }
};

export default {
    getAllSiteDrop,
    createNewSite,
    filterSiteData,
    getOneSiteById,
    updateSiteData
};
