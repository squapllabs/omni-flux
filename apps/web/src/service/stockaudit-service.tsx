import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const filterStockAudit = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/stock-audit/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in stock audit search :', error);
    throw error;
  }
};

const deleteStockAudit = async (id: any) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/stock-audit/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete StockAudit list :', error);
    throw error;
  }
};

const createStockAudit = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/stock-audit/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in StockAudit create :', error);
    throw error;
  }
};

const getOneStockAuditById = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/stock-audit/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne StockAudit data :', error);
    throw error;
  }
};

const updateStockAudits = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/stock-audit/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in updateStockAudits edit:', error);
  }
};

const getAllStockAudits = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/stock-audit/get-all`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all StockAudits:', error);
    throw error;
  }
};

const getItems = async (values : any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-inventory/get-by-project-id-and-site-id/${values?.projectId}/${values?.siteId}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getItems data :', error);
    throw error;
  }
};

export default {
  filterStockAudit,
  deleteStockAudit,
  createStockAudit,
  getOneStockAuditById,
  updateStockAudits,
  getAllStockAudits,
  getItems
};
