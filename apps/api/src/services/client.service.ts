import clientDao from '../dao/client.dao';
import {
  createClientBody,
  updateClientBody,
} from '../interfaces/client.Interface';

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
      result = { success: false, message: 'client_id not found' };
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
      result = { success: false, message: 'client id not exist' };
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

    if (!clientExist) {
      const result = { success: false, message: 'Client Id Not Exist' };
      return result;
    }
    const data = await clientDao.deleteClient(clientId);
    if (data) {
      const result = {
        success: true,
        message: 'Client Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this client',
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteClient client service : ', error);
    throw error;
  }
};

export { createClient, updateClient, getAllClients, getById, deleteClient };
