import prisma from '../utils/prisma';

const add = async (
  name: string,
  type: string,
  mobile_number: string,
  contact_number: string,
  address: JSON,
  description: string,
  created_by: number,
  code: string,
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
        code,
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
  code: string,
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
        code,
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

/* const searchSiteContractor = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterSiteContractor;
    const siteContractor = await transaction.site_contractor.findMany({
      where: filter,
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const siteContractorCount = await transaction.site_contractor.count({
      where: filter,
    });
    const siteContractorData = {
      count: siteContractorCount,
      data: siteContractor,
    };
    return siteContractorData;
  } catch (error) {
    console.log(
      'Error occurred in siteContractor dao : searchSiteContractor ',
      error
    );
    throw error;
  }
}; */

const searchSiteContractor = async (
  offset,
  limit,
  orderByColumn,
  orderByDirection,
  filters,
  global_search,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterSiteContractor;
    const globalSearch = global_search.toLowerCase();
    const allSiteContractors = await transaction.site_contractor.findMany({
      where: filter,
      orderBy: [{ [orderByColumn]: orderByDirection }],
    });

    const filteredSiteContractors = allSiteContractors.filter(
      (siteContractor) => {
        const lowerName = siteContractor.name
          ? siteContractor.name.toLowerCase()
          : '';
        const lowerStreet =
          siteContractor.address && siteContractor.address.street
            ? siteContractor.address.street.toLowerCase()
            : '';
        const lowerCity =
          siteContractor.address && siteContractor.address.city
            ? siteContractor.address.city.toLowerCase()
            : '';
        const lowerState =
          siteContractor.address && siteContractor.address.state
            ? siteContractor.address.state.toLowerCase()
            : '';
        const lowerCountry =
          siteContractor.address && siteContractor.address.country
            ? siteContractor.address.country.toLowerCase()
            : '';
        const lowerPinCode =
          siteContractor.address && siteContractor.address.pin_code
            ? siteContractor.address.pin_code.toLowerCase()
            : '';
        const lowerDescription = siteContractor.description
          ? siteContractor.description.toLowerCase()
          : '';

        return (
          lowerName.includes(globalSearch) ||
          lowerStreet.includes(globalSearch) ||
          lowerCity.includes(globalSearch) ||
          lowerState.includes(globalSearch) ||
          lowerCountry.includes(globalSearch) ||
          lowerPinCode.includes(globalSearch) ||
          lowerDescription.includes(globalSearch)
        );
      }
    );

    const siteContractorCount = filteredSiteContractors.length;
    const pagedSiteContractors = filteredSiteContractors.slice(
      offset,
      offset + limit
    );

    const siteContractorData = {
      count: siteContractorCount,
      data: pagedSiteContractors,
    };

    return siteContractorData;
  } catch (error) {
    console.log(
      'Error occurred in siteContractor dao: searchSiteContractor',
      error
    );
    throw error;
  }
};

const getByCode = async (code: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.findFirst({
      where: {
        code: code,
      },
    });
    return siteContractor;
  } catch (error) {
    console.log('Error occurred in siteContractor getByCode dao', error);
    throw error;
  }
};

const getBySiteId = async (getBySiteId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteContractor = await transaction.site_contractor.findFirst({
      where: {
        site_contractor_id: Number(getBySiteId),
        type: 'Site',
        is_delete: false,
      },
    });

    return siteContractor;
  } catch (error) {
    console.log('Error occurred in siteContractor getBySiteId dao', error);
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
  searchSiteContractor,
  getByCode,
  getBySiteId,
};
