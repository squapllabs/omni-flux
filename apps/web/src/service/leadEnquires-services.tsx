import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllleadEnquiry = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/lead-enquiry/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all lead-enquiry:', error);
    throw error;
  }
};

const getOneleadEnquiryByID = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/lead-enquiry/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne lead-enquiry :', error);
    throw error;
  }
};

const createleadEnquiry = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/lead-enquiry/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in lead-enquiry create :', error);
    throw error;
  }
};
const updateleadEnquiry = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/lead-enquiry/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in lead-enquiry edit:', error);
  }
};
const deleteleadEnquiry = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/lead-enquiry/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete lead-enquiry  :', error);
    throw error;
  }
};

const filterLeadEnquiry = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/lead-enquiry/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in lead filter :', error);
    throw error;
  }
};

const checkDublicateTenderReg = async (value: string) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/lead-enquiry/check-duplicate-tender-reg-no/${value}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in checkDublicateTenderReg   :', error);
    throw error;
  }
};
const checkDublicateTenderIdentiNo = async (value: number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/lead-enquiry/check-duplicate-tender-identification-no/${value}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in checkDublicateTenderIdentiNo   :', error);
    throw error;
  }
};

export default {
  getAllleadEnquiry,
  getOneleadEnquiryByID,
  createleadEnquiry,
  updateleadEnquiry,
  deleteleadEnquiry,
  filterLeadEnquiry,
  checkDublicateTenderReg,
  checkDublicateTenderIdentiNo,
};
