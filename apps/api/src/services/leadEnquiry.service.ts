import leadEnquiryDao from '../dao/leadEnquiry.dao';
import {
  createLeadEnquiryBody,
  updateLeadEnquiryBody,
} from '../interfaces/leadEnquiry.Interface';
import clientDao from '../dao/client.dao';
import prisma from '../utils/prisma';
import leadProductDao from '../dao/leadProduct.dao';
import leadTenderDao from '../dao/leadTender.dao';
import masterDataDao from '../dao/masterData.dao';
import userDao from '../dao/user.dao';

/**
 * Method to Create a New LeadEnquiry
 * @param body
 * @returns
 */
const createLeadEnquiry = async (body: createLeadEnquiryBody) => {
  try {
    const {
      lead_type,
      client,
      client_level,
      client_contact_name,
      client_contact_email,
      client_contact_phone,
      our_remarks,
      client_remark,
      doc_url,
      created_by,
      status_remarks,
      source_name,
      status,
      probability,
      approx_value,
      sales_person_name,
      /* tender_reg_no,
      tender_identification_no, */
      tender_name,
      tender_issue_date,
      tender_due_date,
      tender_type,
      estimated_value,
      industry_sector,
      product_item,
    } = body;
    let result = null;
    const leadCodeData = await generateLeadCode(lead_type);
    const lead_code = leadCodeData.data;

    if (client) {
      const clientExist = await clientDao.getById(client);
      if (!clientExist) {
        result = {
          message: 'client does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (industry_sector) {
      const industrySectorExist = await masterDataDao.getById(industry_sector);
      if (!industrySectorExist) {
        result = {
          message: 'industry_sector does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (probability) {
      const probabilityExist = await masterDataDao.getById(probability);
      if (!probabilityExist) {
        result = {
          message: 'probability does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (client_level) {
      const clientLevelExist = await masterDataDao.getById(client_level);
      if (!clientLevelExist) {
        result = {
          message: 'client_level does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (sales_person_name) {
      const salesPersonNameExist = await userDao.getById(sales_person_name);
      if (!salesPersonNameExist) {
        result = {
          message: 'sales_person_name does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (status) {
      if (!allowedStatusValues.includes(status)) {
        result = {
          message: 'Invalid status value. Allowed values: AWARDED, REJECTED, CLONE, COMPLETED, INPROGRESS, ON HOLD',
          status: false,
          data: null,
        };
        return result;
      }
    }
    let tender_reg_no = null;
    let tender_identification_no = null;
    if (lead_type === 'Tender') {
      const tenderIdAndRegNo = await generateTenderIdAndRegNo();
      tender_reg_no = tenderIdAndRegNo?.data?.tender_reg_no;
      tender_identification_no =
        tenderIdAndRegNo?.data?.tender_identification_no;
    }

    result = await prisma
      .$transaction(async (prisma) => {
        const leadEnquiryDetails = await leadEnquiryDao.add(
          lead_type,
          lead_code,
          client,
          client_level,
          client_contact_name,
          client_contact_email,
          client_contact_phone,
          our_remarks,
          client_remark,
          doc_url,
          created_by,
          status_remarks,
          source_name,
          status,
          probability,
          approx_value,
          sales_person_name,
          tender_reg_no,
          tender_identification_no,
          tender_name,
          tender_issue_date,
          tender_due_date,
          tender_type,
          estimated_value,
          industry_sector,
          product_item,
          prisma
        );

        return leadEnquiryDetails;
      })
      .then((data) => {
        console.log('Successfully Lead Enquiry Data Returned ', data);
        const newLeadEnquiryData = {
          message: 'success',
          status: true,
          data: data,
        };
        return newLeadEnquiryData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in leadEnquiry service Add: ', error);
    throw error;
  }
};

/**
 * Function for Status Value Control
 */
const allowedStatusValues = [
  'AWARDED',
  'REJECTED',
  'CLONE',
  'COMPLETED',
  'INPROGRESS',
  'ON HOLD',
];

/**
 * Function to generate sequential lead code
 * @param leadType
 * @returns
 */
const generateLeadCode = async (leadType) => {
  const leadTypePrefix =
    leadType === 'Product' ? 'LD-PS-' : leadType === 'Tender' ? 'LD-TR-' : 'LD';

  const latestLeadEnquiry = await prisma.lead_enquiry.findFirst({
    where: { lead_type: leadType },
    orderBy: { lead_enquiry_id: 'desc' },
  });

  let sequentialNumber = 1;
  if (latestLeadEnquiry) {
    const latestLeadCode = latestLeadEnquiry.lead_code;
    const latestSequentialNumber = parseInt(
      latestLeadCode.slice(leadTypePrefix.length),
      10
    );
    if (!isNaN(latestSequentialNumber)) {
      sequentialNumber = latestSequentialNumber + 1;
    }
  }
  const leadCode = leadTypePrefix + sequentialNumber;
  const result = { message: 'success', status: true, data: leadCode };
  return result;
};

/**
 * Function to generate Tender Identification No and Tender Reg No
 * @returns
 */
const generateTenderIdAndRegNo = async () => {
  const latestLeadEnquiry = await prisma.lead_enquiry.findFirst({
    where: { lead_type: 'Tender' },
    orderBy: { lead_enquiry_id: 'desc' },
  });

  let sequentialNumber = 1;
  if (latestLeadEnquiry) {
    const latestLeadCode = latestLeadEnquiry.lead_code;
    const latestSequentialNumber = parseInt(
      latestLeadCode.slice('Tender'.length),
      10
    );
    if (!isNaN(latestSequentialNumber)) {
      sequentialNumber = latestSequentialNumber + 1;
    }
  }

  const data = {
    tender_reg_no: `TN-HD/TR/REGN/${sequentialNumber}`,
    tender_identification_no: `TENDER-ID-${sequentialNumber}`,
  };

  const result = { message: 'success', status: true, data: data };
  return result;
};

/**
 * Method to Update an Existing LeadEnquiry
 * @param body
 * @returns
 */
const updateLeadEnquiry = async (body: updateLeadEnquiryBody) => {
  try {
    const {
      lead_type,
      client,
      client_level,
      client_contact_name,
      client_contact_email,
      client_contact_phone,
      our_remarks,
      client_remark,
      doc_url,
      updated_by,
      source_name,
      status,
      status_remarks,
      lead_enquiry_id,
      probability,
      approx_value,
      sales_person_name,
      tender_reg_no,
      tender_identification_no,
      tender_name,
      tender_issue_date,
      tender_due_date,
      tender_type,
      estimated_value,
      industry_sector,
      lead_product_id,
      lead_tender_id,
      product_item,
    } = body;
    let result = null;
    const leadEnquiryExist = await leadEnquiryDao.getById(lead_enquiry_id);

    if (client) {
      const clientExist = await clientDao.getById(client);
      if (!clientExist) {
        const result = {
          message: 'client does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (industry_sector) {
      const industrySectorExist = await masterDataDao.getById(industry_sector);
      if (!industrySectorExist) {
        result = {
          message: 'industry_sector does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (probability) {
      const probabilityExist = await masterDataDao.getById(probability);
      if (!probabilityExist) {
        result = {
          message: 'probability does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (client_level) {
      const clientLevelExist = await masterDataDao.getById(client_level);
      if (!clientLevelExist) {
        result = {
          message: 'client_level does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (sales_person_name) {
      const salesPersonNameExist = await userDao.getById(sales_person_name);
      if (!salesPersonNameExist) {
        result = {
          message: 'sales_person_name does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (lead_product_id) {
      const leadProductExist = await leadProductDao.getById(lead_product_id);
      if (!leadProductExist) {
        const result = {
          message: 'lead_product_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (lead_tender_id) {
      const leadTenderExist = await leadTenderDao.getById(lead_tender_id);
      if (!leadTenderExist) {
        const result = {
          message: 'lead_tender_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (status) {
      if (!allowedStatusValues.includes(status)) {
        result = {
          message: 'Invalid status value. Allowed values: AWARDED, REJECTED, CLONE, COMPLETED, INPROGRESS, ON HOLD',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (leadEnquiryExist) {
      result = await prisma
        .$transaction(async (prisma) => {
          const leadEnquiryDetails = await leadEnquiryDao.edit(
            lead_type,
            client,
            client_level,
            client_contact_name,
            client_contact_email,
            client_contact_phone,
            our_remarks,
            client_remark,
            doc_url,
            updated_by,
            source_name,
            status,
            status_remarks,
            lead_enquiry_id,
            probability,
            approx_value,
            sales_person_name,
            tender_reg_no,
            tender_identification_no,
            tender_name,
            tender_issue_date,
            tender_due_date,
            tender_type,
            estimated_value,
            industry_sector,
            lead_product_id,
            lead_tender_id,
            product_item,
            prisma
          );

          return leadEnquiryDetails;
        })
        .then((data) => {
          console.log('Successfully Lead Enquiry Data Returned ', data);
          const newLeadEnquiryData = {
            message: 'success',
            status: true,
            data: data,
          };
          return newLeadEnquiryData;
        })
        .catch((error: string) => {
          console.log('Failure, ROLLBACK was executed', error);
          throw error;
        });
      return result;
    } else {
      result = {
        message: 'lead_enquiry_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in leadEnquiry service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get LeadEnquiry By LeadEnquiryId
 * @param leadEnquiryId
 * @returns
 */
const getById = async (leadEnquiryId: number) => {
  try {
    let result = null;
    const leadEnquiryData = await leadEnquiryDao.getById(leadEnquiryId);
    if (leadEnquiryData) {
      result = { message: 'success', status: true, data: leadEnquiryData };
      return result;
    } else {
      result = {
        message: 'lead_enquiry_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById leadEnquiry service : ', error);
    throw error;
  }
};

/**
 * Method to Get All LeadEnquiry's
 * @returns
 */
const getAllLeadEnquiry = async () => {
  try {
    const result = await leadEnquiryDao.getAll();
    const leadEnquiryData = { message: 'success', status: true, data: result };
    return leadEnquiryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllLeadEnquiry leadEnquiry service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete leadEnquiry
 * @param leadEnquiryId
 */
const deleteLeadEnquiry = async (leadEnquiryId: number) => {
  try {
    const leadEnquiryExist = await leadEnquiryDao.getById(leadEnquiryId);
    if (!leadEnquiryExist) {
      const result = {
        message: 'lead_enquiry_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (leadEnquiryExist.lead_enquiry_product.length === 0) {
      const result = {

        message: 'Unable to delete.The lead_enquiry_id is mapped in lead_enquiry_product table',
        status: false,
        data: null,
      };
      return result;
    }

    if (leadEnquiryExist.lead_enquiry_tenders.length === 0) {
      const result = {
        message: 'Unable to delete.The lead_enquiry_id is mapped in lead_enquiry_tender table',
        status: false,
        data: null,
      };
      return result;
    }

    if (leadEnquiryExist.lead_enquiry_product_item.length === 0) {
      const result = {
        message: 'Unable to delete.The lead_enquiry_id is mapped in lead_enquiry_product_item table',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await leadEnquiryDao.deleteLeadEnquiry(leadEnquiryId);
    if (data) {
      const result = {
        status: true,
        message: 'LeadEnquiry Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this leadEnquiry',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteLeadEnquiry leadEnquiry service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search LeadEnquiry - Pagination API
 * @returns
 */
const searchLeadEnquiry = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const type = body.type;
    const status = body.status;

    const filterObj: any = {};

    if (status) {
      filterObj.filterLeadEnquiry = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (type) {
      filterObj.filterLeadEnquiry = filterObj.filterLeadEnquiry || {};
      filterObj.filterLeadEnquiry.AND = filterObj.filterLeadEnquiry.AND || [];

      filterObj.filterLeadEnquiry.AND.push({
        lead_type: {
          contains: type,
          mode: 'insensitive',
        },
      });
    }

    if (global_search) {
      filterObj.filterLeadEnquiry = filterObj.filterLeadEnquiry || {};
      filterObj.filterLeadEnquiry.OR = filterObj.filterLeadEnquiry.OR || [];

      filterObj.filterLeadEnquiry.OR.push(
        {
          lead_type: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          lead_code: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          client_contact_name: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          client_contact_email: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          client_contact_phone: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          our_remarks: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          client_remark: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          status: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          status_remarks: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          client_info: {
            name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          client_level_info: {
            master_data_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );

      filterObj.filterLeadEnquiry.OR.push({
        OR: [
          {
            lead_enquiry_product: {
              some: {
                source_name: {
                  contains: global_search,
                  mode: 'insensitive',
                },
              },
            },
          },
          {
            lead_enquiry_product: {
              some: {
                probability_details: {
                  master_data_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
          {
            lead_enquiry_product: {
              some: {
                sales_person_details: {
                  first_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                  last_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
          {
            lead_enquiry_product: {
              some: {
                lead_enquiry_product_item: {
                  some: {
                    product: {
                      item_name: {
                        contains: global_search,
                        mode: 'insensitive',
                      },
                    },
                  },
                },
              },
            },
          },
          {
            lead_enquiry_tenders: {
              some: {
                tender_name: {
                  contains: global_search,
                  mode: 'insensitive',
                },
                tender_reg_no: {
                  contains: global_search,
                  mode: 'insensitive',
                },
                tender_identification_no: {
                  contains: global_search,
                  mode: 'insensitive',
                },
                tender_type: {
                  contains: global_search,
                  mode: 'insensitive',
                },
              },
            },
          },
          {
            lead_enquiry_tenders: {
              some: {
                industry_sector_data: {
                  master_data_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
        ],
      });
    }

    const result = await leadEnquiryDao.searchLeadEnquiry(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempLeadEnquiryData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempLeadEnquiryData;
  } catch (error) {
    console.log(
      'Error occurred in searchLeadEnquiry leadEnquiry service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Check Duplicate Tender Reg No Exist
 * @param leadTenderRegNo
 * @returns
 */
const checkDuplicateTenderRegNo = async (leadTenderRegNo: string) => {
  try {
    let result = null;
    const leadEnquiryData = await leadTenderDao.checkDuplicateTenderRegNo(
      leadTenderRegNo
    );
    if (leadEnquiryData) {
      result = {
        message: 'This tender_reg_no is already exist',
        status: true,
        is_exist: true,
        data: leadEnquiryData,
      };
      return result;
    } else {
      result = {
        message: 'This tender_reg_no does not exist',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in checkDuplicateTenderRegNo leadEnquiry service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Check Duplicate Tender Identification No
 * @param leadTenderRegNo
 * @returns
 */
const checkDuplicateTenderIdentificationNo = async (
  leadTenderIdentificationNo: string
) => {
  try {
    let result = null;
    const leadEnquiryData =
      await leadTenderDao.checkDuplicateTenderIdentificationNo(
        leadTenderIdentificationNo
      );
    if (leadEnquiryData) {
      result = {
        message: 'This tender_identification_no is already exist',
        status: true,
        is_exist: true,
        data: leadEnquiryData,
      };
      return result;
    } else {
      result = {
        message: 'This tender_identification_no does not exist',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in checkDuplicateTenderIdentificationNo leadEnquiry service : ',
      error
    );
    throw error;
  }
};

export {
  createLeadEnquiry,
  updateLeadEnquiry,
  getAllLeadEnquiry,
  getById,
  deleteLeadEnquiry,
  searchLeadEnquiry,
  checkDuplicateTenderRegNo,
  checkDuplicateTenderIdentificationNo,
  generateLeadCode,
  generateTenderIdAndRegNo,
};
