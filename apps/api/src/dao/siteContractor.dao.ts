import prisma from '../utils/prisma';

const add = async (
  name: string,
  type: string,
  mobile_number: string,
  contact_number: string,
  address: JSON,
  description: string,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.create({
      data: {
        name,
        type,
        mobile_number,
        contact_number,
        address,
        description,
        created_by,
        is_delete: is_delete,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return siteContractor;
  } catch (error) {
    console.log('Error occurred in siteContractorDao add', error);
    throw error;
  }
};

const edit = async (
  name: string,
  type: string,
  mobile_number: string,
  contact_number: string,
  address: JSON,
  description: string,
  updated_by: number,
  site_contractor_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.update({
      where: {
        site_contractor_id: site_contractor_id,
      },
      data: {
        name,
        type,
        mobile_number,
        contact_number,
        address,
        description,
        updated_by,
        updated_date: currentDate,
      },
    });
    return siteContractor;
  } catch (error) {
    console.log('Error occurred in siteContractorDao edit', error);
    throw error;
  }
};

const getById = async (siteContractorId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.findFirst({
      where: {
        site_contractor_id: Number(siteContractorId),
        is_delete: false,
      },
    });

    return siteContractor;
  } catch (error) {
    console.log('Error occurred in siteContractor getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.findMany({
      where: {
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return siteContractor;
  } catch (error) {
    console.log('Error occurred in siteContractor getAll dao', error);
    throw error;
  }
};

const getAllSites = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.findMany({
      where: {
        type: 'Site',
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return siteContractor;
  } catch (error) {
    console.log('Error occurred in siteContractor getAllSites dao', error);
    throw error;
  }
};

const getAllContractors = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.findMany({
      where: {
        type: 'Contractor',
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return siteContractor;
  } catch (error) {
    console.log(
      'Error occurred in siteContractor getAllContractors dao',
      error
    );
    throw error;
  }
};

const deleteSiteContractor = async (
  siteContractorId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.update({
      where: {
        site_contractor_id: Number(siteContractorId),
      },
      data: {
        is_delete: true,
      },
    });
    return siteContractor;
  } catch (error) {
    console.log(
      'Error occurred in siteContractor deleteSiteContractor dao',
      error
    );
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteSiteContractor,
  getAllSites,
  getAllContractors,
};
