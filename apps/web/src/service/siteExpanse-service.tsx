import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllsiteExpense = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/site-expense/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all site-expense:', error);
    throw error;
  }
};

const getOnesiteExpenseByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/site-expense/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne site-expense :', error);
    throw error;
  }
};

const createsiteExpense = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/site-expense/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in site-expense create :', error);
    throw error;
  }
};
const updatesiteExpense = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/site-expense/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in site-expense edit:', error);
  }
};
const deletesiteExpense = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/site-expense/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete site-expense  :', error);
    throw error;
  }
};

const filtersiteExpense = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/site-expense/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in lead filter :', error);
    throw error;
  }
};

const getSiteExpenseByProjectandSiteID = async (value: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/site-expense/get-by-project-id-and-site-id/${value.projectId}/${value.siteId}`
    );
    return response.data;
  } catch (error) {
    console.log(
      'Error in occur in getSiteExpenseByProjectandSiteID   :',
      error
    );
    throw error;
  }
};
const bulkUploadSiteExpanse = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/site-expense-details/add-bulk`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in site-expense bulkUploadSiteExpanse :', error);
    throw error;
  }
};

export default {
  getAllsiteExpense,
  getOnesiteExpenseByID,
  createsiteExpense,
  updatesiteExpense,
  deletesiteExpense,
  filtersiteExpense,
  bulkUploadSiteExpanse,
  getSiteExpenseByProjectandSiteID,
};
