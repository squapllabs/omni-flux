import prisma from '../utils/prisma';
// import { CaseInsensitiveFilter } from '../utils/caseSensitiveFilter';
import customQueryExecutor from './common/utils.dao';

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
  is_delete,
  global_search
  // connectionObj = null
) => {
  try {
    // const transaction = connectionObj !== null ? connectionObj : prisma;

    const query = `
      SELECT *
      FROM vendor v
      LEFT JOIN master_data md ON md.master_data_id = v.preferred_payment_method_id
      LEFT JOIN master_data md2 ON md2.master_data_id = v.vendor_category_id
      WHERE v.is_delete = ${is_delete}
      AND (
        v.vendor_name ILIKE '%${global_search}%'
        OR v.contact_person ILIKE '%${global_search}%' 
        OR v.contact_email ILIKE '%${global_search}%'
        OR v.contact_phone_no ILIKE '%${global_search}%'
        OR v.address ->> 'street' ILIKE '%${global_search}%'
        OR v.address ->> 'city' ILIKE '%${global_search}%'
        OR v.address ->> 'state' ILIKE '%${global_search}%'
        OR v.address ->> 'country' ILIKE '%${global_search}%'
        OR v.address ->> 'pin_code' ILIKE '%${global_search}%'
        OR v.tax_id ILIKE '%${global_search}%'
        OR v.payment_terms ILIKE '%${global_search}%'
        OR md.master_data_name ILIKE '%${global_search}%'
        OR v.bank_account_details ->> 'bank_name' ILIKE '%${global_search}%'
        OR v.bank_account_details ->> 'ifsc_code' ILIKE '%${global_search}%'
        OR v.bank_account_details ->> 'account_no' ILIKE '%${global_search}%'
        OR v.bank_account_details ->> 'acc_holder_name' ILIKE '%${global_search}%'
        OR v.currency ILIKE '%${global_search}%'
        OR md2.master_data_name ILIKE '%${global_search}%'
        OR v.lead_time ILIKE '%${global_search}%'
        OR v.notes ILIKE '%${global_search}%'
      )
      ORDER BY v.${orderByColumn} ${orderByDirection}
      LIMIT ${limit}
      OFFSET ${offset}`;

    const countQuery = `    SELECT count(*)
      FROM vendor v
      LEFT JOIN master_data md ON md.master_data_id = v.preferred_payment_method_id
      LEFT JOIN master_data md2 ON md2.master_data_id = v.vendor_category_id
      WHERE v.is_delete = ${is_delete}
      AND (
        v.vendor_name ILIKE '%${global_search}%'
        OR v.contact_person ILIKE '%${global_search}%' 
        OR v.contact_email ILIKE '%${global_search}%'
        OR v.contact_phone_no ILIKE '%${global_search}%'
        OR v.address ->> 'street' ILIKE '%${global_search}%'
        OR v.address ->> 'city' ILIKE '%${global_search}%'
        OR v.address ->> 'state' ILIKE '%${global_search}%'
        OR v.address ->> 'country' ILIKE '%${global_search}%'
        OR v.address ->> 'pin_code' ILIKE '%${global_search}%'
        OR v.tax_id ILIKE '%${global_search}%'
        OR v.payment_terms ILIKE '%${global_search}%'
        OR md.master_data_name ILIKE '%${global_search}%'
        OR v.bank_account_details ->> 'bank_name' ILIKE '%${global_search}%'
        OR v.bank_account_details ->> 'ifsc_code' ILIKE '%${global_search}%'
        OR v.bank_account_details ->> 'account_no' ILIKE '%${global_search}%'
        OR v.bank_account_details ->> 'acc_holder_name' ILIKE '%${global_search}%'
        OR v.currency ILIKE '%${global_search}%'
        OR md2.master_data_name ILIKE '%${global_search}%'
        OR v.lead_time ILIKE '%${global_search}%'
        OR v.notes ILIKE '%${global_search}%'
      )`;

    const result = await customQueryExecutor.customQueryExecutor(query);
    const count = await customQueryExecutor.customQueryExecutor(countQuery);

    const vendorData = { count: Number(count[0].count), data: result };
    return vendorData;

    // const result = await transaction.$queryRaw`      SELECT *
    // FROM vendor v
    // LEFT JOIN master_data md ON md.master_data_id = v.preferred_payment_method_id
    // LEFT JOIN master_data md2 ON md2.master_data_id = v.vendor_category_id
    // WHERE v.is_delete = true
    // AND (
    //   v.vendor_name ILIKE '%' || ${global_search} || '%'
    //   OR v.contact_person ILIKE '%' || ${global_search} || '%'
    //   OR v.contact_email ILIKE '%' || ${global_search} || '%'
    //   OR v.contact_phone_no ILIKE '%' || ${global_search} || '%'
    //   OR v.address ->> 'street' ILIKE '%' || ${global_search} || '%'
    //   OR v.address ->> 'city' ILIKE '%' || ${global_search} || '%'
    //   OR v.address ->> 'state' ILIKE '%' || ${global_search} || '%'
    //   OR v.address ->> 'country' ILIKE '%' || ${global_search} || '%'
    //   OR v.address ->> 'pin_code' ILIKE '%' || ${global_search} || '%'
    //   OR v.tax_id ILIKE '%' || ${global_search} || '%'
    //   OR v.payment_terms ILIKE '%' || ${global_search} || '%'
    //   OR md.master_data_name ILIKE '%' || ${global_search} || '%'
    //   OR v.bank_account_details ->> 'bank_name' ILIKE '%' || ${global_search} || '%'
    //   OR v.bank_account_details ->> 'ifsc_code' ILIKE '%' || ${global_search} || '%'
    //   OR v.bank_account_details ->> 'account_no' ILIKE '%' || ${global_search} || '%'
    //   OR v.bank_account_details ->> 'acc_holder_name' ILIKE '%' || ${global_search} || '%'
    //   OR v.currency ILIKE '%' || ${global_search} || '%'
    //   OR md2.master_data_name ILIKE '%' || ${global_search} || '%'
    //   OR v.lead_time ILIKE '%' || ${global_search} || '%'
    //   OR v.notes ILIKE '%' || ${global_search} || '%'
    // )
    // ORDER BY ${orderByColumn} desc
    // LIMIT ${limit}
    // OFFSET ${offset}`;

    // return result;
  } catch (error) {
    console.error('Error occurred in vendor dao: searchVendor', error);
    throw error;
  }
};

// const searchVendor = async (
//   offset: number,
//   limit: number,
//   orderByColumn: string,
//   orderByDirection: string,
//   filters,
//   global_search,
//   connectionObj = null
// ) => {
//   try {
//     const transaction = connectionObj !== null ? connectionObj : prisma;

//     const query = `select
//     *
//   from
//     vendor v
//   left join master_data md on
//     md.master_data_id = v.preferred_payment_method_id
//   left join master_data md2 on
//     md2.master_data_id = v.vendor_category_id
//   where
//     v.is_delete = true
//     and
//   (
//     v.vendor_name ilike '%' || ${global_search}|| '%'
//     or v.contact_person ilike '%' || ${global_search}|| '%'
//     or v.contact_email ilike '%' || ${global_search}|| '%'
//     or v.contact_phone_no ilike '%' || ${global_search}|| '%'
//     or v.address ->>'street' ilike '%' || ${global_search}|| '%'
//     or v.address ->>'city' ilike '%' || ${global_search}|| '%'
//     or v.address ->>'state' ilike '%' || ${global_search}|| '%'
//     or v.address ->>'country' ilike '%' || ${global_search}|| '%'
//     or v.address ->>'pin_code' ilike '%' || ${global_search}|| '%'
//     or v.tax_id ilike '%' || ${global_search}|| '%'
//     or v.payment_terms ilike '%' || ${global_search}|| '%'
//     or md.master_data_name ilike '%' || ${global_search}|| '%'
//     or v.bank_account_details ->>'bank_name' ilike '%' || ${global_search}|| '%'
//     or v.bank_account_details ->>'ifsc_code' ilike '%' || ${global_search}|| '%'
//     or v.bank_account_details ->>'account_no' ilike '%' || ${global_search}|| '%'
//     or v.bank_account_details ->>'acc_holder_name' ilike '%' || ${global_search}|| '%'
//     or v.currency ilike '%' || ${global_search}|| '%'
//     or md2.master_data_name ilike '%' || ${global_search}|| '%'
//     or v.lead_time ilike '%' || ${global_search}|| '%'
//     or v.notes ilike '%' || ${global_search}|| '%'
//     )
//   order by
//     ${orderByColumn} ${orderByDirection}`;

//     console.log('query ', query);

//     const vendor = await transaction.$queryRaw`select
//       *
//     from
//       vendor v
//     left join master_data md on
//       md.master_data_id = v.preferred_payment_method_id
//     left join master_data md2 on
//       md2.master_data_id = v.vendor_category_id
//     where
//       v.is_delete = true
//       and
//     (
//       v.vendor_name ilike '%' || ${global_search}|| '%'
//       or v.contact_person ilike '%' || ${global_search}|| '%'
//       or v.contact_email ilike '%' || ${global_search}|| '%'
//       or v.contact_phone_no ilike '%' || ${global_search}|| '%'
//       or v.address ->>'street' ilike '%' || ${global_search}|| '%'
//       or v.address ->>'city' ilike '%' || ${global_search}|| '%'
//       or v.address ->>'state' ilike '%' || ${global_search}|| '%'
//       or v.address ->>'country' ilike '%' || ${global_search}|| '%'
//       or v.address ->>'pin_code' ilike '%' || ${global_search}|| '%'
//       or v.tax_id ilike '%' || ${global_search}|| '%'
//       or v.payment_terms ilike '%' || ${global_search}|| '%'
//       or md.master_data_name ilike '%' || ${global_search}|| '%'
//       or v.bank_account_details ->>'bank_name' ilike '%' || ${global_search}|| '%'
//       or v.bank_account_details ->>'ifsc_code' ilike '%' || ${global_search}|| '%'
//       or v.bank_account_details ->>'account_no' ilike '%' || ${global_search}|| '%'
//       or v.bank_account_details ->>'acc_holder_name' ilike '%' || ${global_search}|| '%'
//       or v.currency ilike '%' || ${global_search}|| '%'
//       or md2.master_data_name ilike '%' || ${global_search}|| '%'
//       or v.lead_time ilike '%' || ${global_search}|| '%'
//       or v.notes ilike '%' || ${global_search}|| '%'
//       )
//     order by
//       ${orderByColumn} ${orderByDirection}`;

//     return vendor;

//     /* const filter = filters.filterVendor; */

//     /*     const vendor = await transaction.vendor.findMany({
//       where: filter,
//       include: {
//         vendor_category_data: true,
//         preferred_payment_method_data: true,
//       },
//       orderBy: [
//         {
//           [orderByColumn]: orderByDirection,
//         },
//       ],
//     });

//     const globalSearch = global_search;

//     const propertiesToFilter = [
//       'preferred_payment_method_data.master_data_name',
//       'vendor_category_data.master_data_name',
//       'notes',
//       'lead_time',
//       'currency',
//       'payment_terms',
//       'tax_id',
//       'contact_phone_no',
//       'contact_email',
//       'contact_person',
//       'bank_account_details.bank_name',
//       'bank_account_details.account_number',
//       'address.street',
//       'address.city',
//       'address.state',
//       'address.country',
//       'address.pin_code',
//       'bank_account_details.ifsc_code',
//       'vendor_name',
//     ];

//     const filteredVendors = CaseInsensitiveFilter(
//       vendor,
//       globalSearch,
//       propertiesToFilter
//     );
//     const vendorCount = filteredVendors.length;
//     const vendors = filteredVendors.slice(offset, offset + limit);
//     const vendorData = {
//       count: vendorCount,
//       data: vendors,
//     };
//     return vendorData; */
//   } catch (error) {
//     console.log('Error occurred in vendor dao : searchVendor ', error);
//     throw error;
//   }
// };

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
