import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getPurchaseRegisterReport = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/purchase-order/get-po-report/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in getPurchaseRegisterReport  :', error);
    throw error;
  }
};

const projectInwardReport = async (values: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-inventory/get-by-project-id/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in projectInwardReport  :', error);
    throw error;
  }
};

const getPurchaseRequestReport = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/purchase-request/get-pr-report/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in getPurchaseRequestReport  :', error);
    throw error;
  }
};

const getPurchaseOrderInvoiceReport = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/purchase-order-invoice/search/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in getPurchaseOrderInvoiceReport  :', error);
    throw error;
  }
};


export default {
  getPurchaseRegisterReport,
  projectInwardReport,
  getPurchaseRequestReport,
  getPurchaseOrderInvoiceReport
};
