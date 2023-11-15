import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllsiteExpense = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/expense/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all expense:', error);
    throw error;
  }
};

const getOnesiteExpenseByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/expense/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne expense :', error);
    throw error;
  }
};

const getOnesiteExpenseByCode = async (code: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/expense/get-by-expense-code/${code}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne expense :', error);
    throw error;
  }
};

const getOnesiteExpenseDetailByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/expense-details/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne expense detail :', error);
    throw error;
  }
};

const createsiteExpense = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/expense/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in expense create :', error);
    throw error;
  }
};

const createGlobalExpense = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/expense/addIndependentExpense/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in expense create :', error);
    throw error;
  }
};

const updatesiteExpense = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/expense/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in expense edit:', error);
  }
};

const updateGlobalExpense = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/expense/updateIndependentExpense/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in expense edit:', error);
  }
};

const updatesiteExpenseStatus = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/expense/update-status`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in expense edit:', error);
  }
};

const updatesiteExpenseDetail = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/expense-details/update-status`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in expense detail edit:', error);
  }
};
const deletesiteExpense = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/expense/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete expense  :', error);
    throw error;
  }
};

const filtersiteExpense = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/expense/search`,
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
      `${environment.apiUrl}/expense/get-by-project-id-and-site-id/${value.projectId}/${value.siteId}`
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
      `${environment.apiUrl}/expense-details/add-bulk`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in expense bulkUploadSiteExpanse :', error);
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
  updatesiteExpenseDetail,
  getOnesiteExpenseDetailByID,
  updatesiteExpenseStatus,
  getOnesiteExpenseByCode,
  createGlobalExpense,
  updateGlobalExpense
};
