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

const searchSiteContractor = async (
  offset,
  limit,
  orderByColumn,
  orderByDirection,
  global_search,
  type,
  status,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const globalSearch = global_search.toLowerCase();
    const types = type;
    const order_by_column = orderByColumn;
    const order_by_direction = orderByDirection;
    const is_delete = status === 'AC' ? false : true;
    const getData = await transaction.site_contractor.findMany({});
    if (getData.length > 0) {
      const allSiteContractors = await transaction.$queryRawUnsafe(
        `SELECT *,
      CAST(created_by AS int) AS created_by,
      CAST(updated_by AS int) AS updated_by
      FROM site_contractor
      WHERE
      (
      (type = '${types}') AND
      (
        name ILIKE '%' || '${globalSearch}' || '%' OR
        description ILIKE '%' || '${globalSearch}' || '%' OR
        address->>'street' ILIKE '%' || '${globalSearch}' || '%' OR
        address->>'city' ILIKE '%' || '${globalSearch}' || '%' OR
        address->>'state' ILIKE '%' || '${globalSearch}' || '%' OR
        address->>'pin_code' ILIKE '%' || '${globalSearch}' || '%' OR
        address->>'country' ILIKE '%' || '${globalSearch}' || '%'
      )
      )
      AND (is_delete = ${is_delete})
      ORDER BY ${order_by_column} ${order_by_direction}
      LIMIT ${limit}
      OFFSET ${offset}`
      );

      const countQuery = await transaction.$queryRaw`
    SELECT count(*)
    FROM site_contractor
    WHERE
    (
    (type = ${types}) AND
    (
      name ILIKE '%' || ${globalSearch} || '%' OR
      description ILIKE '%' || ${globalSearch} || '%' OR
      address->>'street' ILIKE '%' || ${globalSearch} || '%' OR
      address->>'city' ILIKE '%' || ${globalSearch} || '%' OR
      address->>'state' ILIKE '%' || ${globalSearch} || '%' OR
      address->>'pin_code' ILIKE '%' || ${globalSearch} || '%' OR
      address->>'country' ILIKE '%' || ${globalSearch} || '%'
    )
    )
    AND (is_delete = ${is_delete})`;

      const siteContractorData = {
        count: Number(countQuery[0].count),
        data: allSiteContractors,
      };
      return siteContractorData;
    } else {
      return getData;
    }
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
