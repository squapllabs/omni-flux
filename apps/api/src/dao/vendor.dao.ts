import prisma from '../utils/prisma';

const add = async (
  vendor_name: string,
  contact_person: string,
  contact_email: string,
  contact_phone_no: string,
  address: JSON,
  tax_id: string,
  payment_terms: string,
  preferred_payment_method_id: number,
  bank_account_details: JSON,
  currency: string,
  vendor_category_id: number,
  lead_time: string,
  minimum_order_quantity: number,
  notes: string,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendor = await transaction.vendor.create({
      data: {
        vendor_name,
        contact_person,
        contact_email,
        contact_phone_no,
        address,
        tax_id,
        payment_terms,
        preferred_payment_method_id,
        bank_account_details,
        currency,
        vendor_category_id,
        lead_time,
        minimum_order_quantity,
        notes,
        created_by,
        is_delete: is_delete,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return vendor;
  } catch (error) {
    console.log('Error occurred in vendorDao add', error);
    throw error;
  }
};

const edit = async (
  vendor_name: string,
  contact_person: string,
  contact_email: string,
  contact_phone_no: string,
  address: JSON,
  tax_id: string,
  payment_terms: string,
  preferred_payment_method_id: number,
  bank_account_details: JSON,
  currency: string,
  vendor_category_id: number,
  lead_time: string,
  minimum_order_quantity: number,
  notes: string,
  updated_by: number,
  vendor_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendor = await transaction.vendor.update({
      where: {
        vendor_id: vendor_id,
      },
      data: {
        vendor_name,
        contact_person,
        contact_email,
        contact_phone_no,
        address,
        tax_id,
        payment_terms,
        preferred_payment_method_id,
        bank_account_details,
        currency,
        vendor_category_id,
        lead_time,
        minimum_order_quantity,
        notes,
        updated_by,
        updated_date: currentDate,
      },
    });
    return vendor;
  } catch (error) {
    console.log('Error occurred in vendorDao edit', error);
    throw error;
  }
};

const getById = async (vendorId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendor = await transaction.vendor.findFirst({
      where: {
        vendor_id: Number(vendorId),
        is_delete: false,
      },
      include: {
        vendor_category_data: true,
        preferred_payment_method_data: true,
      },
    });
    return vendor;
  } catch (error) {
    console.log('Error occurred in vendor getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendor = await transaction.vendor.findMany({
      where: {
        is_delete: false,
      },
      include: {
        vendor_category_data: true,
        preferred_payment_method_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return vendor;
  } catch (error) {
    console.log('Error occurred in vendor getAll dao', error);
    throw error;
  }
};

const deleteVendor = async (vendorId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendor = await transaction.vendor.update({
      where: {
        vendor_id: Number(vendorId),
      },
      data: {
        is_delete: true,
      },
    });
    return vendor;
  } catch (error) {
    console.log('Error occurred in vendor deleteVendor dao', error);
    throw error;
  }
};

const searchVendor = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  global_search,
  status,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = status === 'AC' ? false : true;
    const order_by_column = orderByColumn;
    const order_by_direction = orderByDirection;
    const globalSearch = global_search;

    const allVendors = await transaction.$queryRawUnsafe(`
      SELECT *,
        CAST(created_by AS int) AS created_by,
        CAST(updated_by AS int) AS updated_by
      FROM vendor
        WHERE
        (
          vendor_name ILIKE '%' || '${globalSearch}' || '%' OR
          contact_email ILIKE '%' || '${globalSearch}' || '%' OR
          tax_id ILIKE '%' || '${globalSearch}' || '%' OR
          payment_terms ILIKE '%' || '${globalSearch}' || '%' OR
          notes ILIKE '%' || '${globalSearch}' || '%' OR
          lead_time ILIKE '%' || '${globalSearch}' || '%' OR
          currency ILIKE '%' || '${globalSearch}' || '%' OR
          address->>'street' ILIKE '%' || '${globalSearch}' || '%' OR
          address->>'city' ILIKE '%' || '${globalSearch}' || '%' OR
          address->>'state' ILIKE '%' || '${globalSearch}' || '%' OR
          address->>'pin_code' ILIKE '%' || '${globalSearch}' || '%' OR
          address->>'country' ILIKE '%' || '${globalSearch}' || '%'  OR
          bank_account_details->>'bank_name' ILIKE '%' || '${globalSearch}' || '%' OR
          bank_account_details->>'ifsc_code' ILIKE '%' || '${globalSearch}' || '%' OR
          bank_account_details->>'acc_holder_name' ILIKE '%' || '${globalSearch}' || '%'
        )
        AND (is_delete = ${is_delete})
        ORDER BY ${order_by_column} ${order_by_direction}
        LIMIT ${limit}
        OFFSET ${offset}`
    );

    const countQuery = await transaction.$queryRaw`
        SELECT *,
        CAST(created_by AS int) AS created_by,
        CAST(updated_by AS int) AS updated_by
        FROM vendor
        WHERE
          (
            vendor_name ILIKE '%' || ${globalSearch} || '%' OR
            contact_person ILIKE '%' || ${globalSearch} || '%' OR
            contact_email ILIKE '%' || ${globalSearch} || '%' OR
            tax_id ILIKE '%' || ${globalSearch} || '%' OR
            payment_terms ILIKE '%' || ${globalSearch} || '%' OR
            notes ILIKE '%' || ${globalSearch} || '%' OR
            lead_time ILIKE '%' || ${globalSearch} || '%' OR
            currency ILIKE '%' || ${globalSearch} || '%' OR
            address->>'street' ILIKE '%' || ${globalSearch} || '%' OR
            address->>'city' ILIKE '%' || ${globalSearch} || '%' OR
            address->>'state' ILIKE '%' || ${globalSearch} || '%' OR
            address->>'pin_code' ILIKE '%' || ${globalSearch} || '%' OR
            address->>'country' ILIKE '%' || ${globalSearch} || '%'  OR
            bank_account_details->>'bank_name' ILIKE '%' || ${globalSearch} || '%' OR
            bank_account_details->>'ifsc_code' ILIKE '%' || ${globalSearch} || '%' OR
            bank_account_details->>'acc_holder_name' ILIKE '%' || ${globalSearch} || '%'
          )
          AND (is_delete = ${is_delete})`;

    const countResult = countQuery.length;
    const vendorData = {
      count: countResult,
      data: allVendors,
    };
    return vendorData;
  } catch (error) {
    console.log('Error occurred in vendor dao : searchVendor ', error);
    throw error;
  }
};

const getByEmailId = async (contact_email: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendor = await transaction.vendor.findFirst({
      where: {
        contact_email: contact_email,
      },
      include: {
        vendor_category_data: true,
        preferred_payment_method_data: true,
      },
    });
    return vendor;
  } catch (error) {
    console.log('Error occurred in vendor getByEmailId dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteVendor,
  searchVendor,
  getByEmailId,
};
