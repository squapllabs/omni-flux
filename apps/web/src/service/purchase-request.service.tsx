import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getOnePurchaseRequest = async (id: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/purchase-request/get/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get by id getOnePurchaseRequest:', error);
    throw error;
  }
};

const createPurchaseOrderItem = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/purchase-order/purchase-order-with-item`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in createPurchaseOrderItem-data create :', error);
    throw error;
  }
};

const getOneOrderPurchaseRequest = async (id: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/purchase-order/get-by-purchase-request-id/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in get by id getOneOrderPurchaseRequest:', error);
    throw error;
  }
};

const getAllBillStatusParentType = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/master-data/get-by-type/BSCTY`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting getAllBillStatusParentType:', error);
    throw error;
  }
};

const getOnePurchaseOrderDataByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/purchase-order/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOnePurchaseOrderDataByID :', error);
    throw error;
  }
};

const updatePoBillStatus = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/purchase-order/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in updatePoBillStatus:', error);
  }
};

const getPoData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/purchase-order/search`,
      values
    );

    return response.data;
  } catch (error) {
    console.log('Error in getPoData filter :', error);
    throw error;
  }
};
export default {
    getOnePurchaseRequest,
    createPurchaseOrderItem,
    getOneOrderPurchaseRequest,
    getAllBillStatusParentType,
    getOnePurchaseOrderDataByID,
    updatePoBillStatus,
    getPoData
};
