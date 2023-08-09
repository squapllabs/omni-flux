import clientDao from '../dao/client.dao';
import {
  createClientBody,
  updateClientBody,
} from '../interfaces/client.Interface';
import projectDao from '../dao/project.dao';
import leadEnquiryDao from '../dao/leadEnquiry.dao';

/**
 * Method to Create a New Client
 * @param body
 * @returns
 */
const createClient = async (body: createClientBody) => {
  try {
    const { name, contact_details, created_by } = body;
    const clientDetails = await clientDao.add(
      name,
      contact_details,
      created_by
    );
    const result = { success: true, data: clientDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in client service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Client
 * @param body
 * @returns
 */

const updateClient = async (body: updateClientBody) => {
  try {
    const { name, contact_details, updated_by, client_id } = body;
    let result = null;
    const clientExist = await clientDao.getById(client_id);
    if (!clientExist) {
      result = { success: false, message: 'client_id does not exist' };
      return result;
    } else {
      const clientDetails = await clientDao.edit(
        name,
        contact_details,
        updated_by,
        client_id
      );
      result = { success: true, data: clientDetails };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in client service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Client By ClientId
 * @param clientId
 * @returns
 */
const getById = async (clientId: number) => {
  try {
    let result = null;
    const clientData = await clientDao.getById(clientId);
    if (clientData) {
      result = { success: true, data: clientData };
      return result;
    } else {
      result = { success: false, message: 'client_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById client service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Clients
 * @returns
 */
const getAllClients = async () => {
  try {
    const result = await clientDao.getAll();
    const clientData = { success: true, data: result };
    return clientData;
  } catch (error) {
    console.log('Error occurred in getAllClients client service : ', error);
    throw error;
  }
};

/**
 * Method to delete client
 * @param clientId
 */
const deleteClient = async (clientId: number) => {
  try {
    const clientExist = await clientDao.getById(clientId);
    const clientExistInProject = await projectDao.getByClientId(clientId);
    const clientExistInLeadEnquiry = await leadEnquiryDao.getByClientId(
      clientId
    );

    if (!clientExist) {
      const result = {
        message: 'client_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (clientExistInProject) {
      const result = {
        message: 'Unable to delete.This client_id is mapped in project table',
        status: false,
        data: null,
      };
      return result;
    }

    if (clientExistInLeadEnquiry) {
      const result = {
        message:
          'Unable to delete.This client_id is mapped in lead_enquiry table',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await clientDao.deleteClient(clientId);
    if (data) {
      const result = {
        message: 'Client Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this client',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteClient client service : ', error);
    throw error;
  }
};

/**
 * Method to search Client - Pagination API
 * @returns
 */
const searchClient = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const status = body.status;
    const filterObj = {
      filterClient: {
        AND: [],
        OR: [
          { name: { contains: global_search, mode: 'insensitive' } },
          { contact_details: { contains: global_search, mode: 'insensitive' } },
        ],
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await clientDao.searchClient(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempClientData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempClientData;
  } catch (error) {
    console.log('Error occurred in searchClient Client service : ', error);
    throw error;
  }
};

export {
  createClient,
  updateClient,
  getAllClients,
  getById,
  deleteClient,
  searchClient,
};
