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

const checkSiteCodeDuplicate = async (values:any) => {
  const data = values.toUpperCase()
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/site-contractor/check-duplicate-code/${data}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in check checkSiteCodeDuplicate :', error);
  }
};

const deleteSite = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/site-contractor/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete gst list :', error);
    throw error;
  }
};
export default {
    getAllSiteDrop,
    createNewSite,
    filterSiteData,
    getOneSiteById,
    updateSiteData,
    checkSiteCodeDuplicate,
    deleteSite
};
