import siteExpenseDao from '../dao/siteExpense.dao';
import siteDao from '../dao/siteContractor.dao';
import projectDao from '../dao/project.dao';
import siteExpenseDetailsDao from '../dao/siteExpenseDetails.dao';
import { siteExpenseDetailsBody } from '../interfaces/siteExpenseDetails.Interface';

/**
 * Method to Create a New siteExpenseDetails
 * @param body
 * @returns
 */
const createSiteExpenseDetails = async (body: siteExpenseDetailsBody) => {
  try {
    const {
      description,
      air_transport,
      fuel,
      labour_advance,
      phone_stationary,
      food_snacks,
      purchase_service,
      others,
      total,
      bill_details,
      project_id,
      site_id,
      created_by,
      status = 'Open',
      comments,
      progressed_by,
    } = body;
    let result = null;

    if (site_id) {
      const siteExist = await siteDao.getBySiteId(site_id);
      if (!siteExist) {
        result = {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        result = {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    let site_expense_id = null;

    if (project_id && site_id) {
      const siteExpenseExist = await siteExpenseDao.getByProjectIdAndSiteId(
        project_id,
        site_id
      );
      if (siteExpenseExist) {
        site_expense_id = siteExpenseExist?.site_expense_id;
      } else {
        const siteExpense = await siteExpenseDao.add(
          site_id,
          project_id,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          created_by,
          []
        );

        site_expense_id = siteExpense?.site_expense?.site_expense_id;
      }
    }

    const siteExpenseDetails = await siteExpenseDetailsDao.add(
      description,
      air_transport,
      fuel,
      labour_advance,
      phone_stationary,
      food_snacks,
      purchase_service,
      others,
      total,
      bill_details,
      site_expense_id,
      created_by,
      status,
      comments,
      progressed_by
    );
    result = { message: 'success', status: true, data: siteExpenseDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in siteExpenseDetails service Add: ', error);
    throw error;
  }
};

/**
 * Method to Create a New siteExpenseDetails
 * @param body
 * @returns
 */
const addBulkSiteExpenseDetails = async (body: siteExpenseDetailsBody) => {
  try {
    const { project_id, site_id, created_by, site_expense_details } = body;
    let result = null;

    if (site_id) {
      const siteExist = await siteDao.getBySiteId(site_id);
      if (!siteExist) {
        result = {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        result = {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    let site_expense_id = null;

    if (project_id && site_id) {
      const siteExpenseExist = await siteExpenseDao.getByProjectIdAndSiteId(
        project_id,
        site_id
      );
      if (siteExpenseExist) {
        site_expense_id = siteExpenseExist?.site_expense_id;
      } else {
        const siteExpense = await siteExpenseDao.add(
          site_id,
          project_id,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          created_by,
          []
        );

        site_expense_id = siteExpense?.site_expense?.site_expense_id;
      }
    }

    const currentDate = new Date();
    const updated_site_expense_details = site_expense_details.map((detail) => {
      return {
        ...detail,
        site_expense_id: site_expense_id,
        is_delete: false,
        created_date: currentDate,
        updated_date: currentDate,
        created_by: created_by,
        status: 'Open',
      };
    });

    const siteExpenseDetailsBulkData = await siteExpenseDetailsDao.addBulk(
      updated_site_expense_details
    );

    result = {
      message: 'success',
      status: true,
      data: siteExpenseDetailsBulkData,
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in siteExpenseDetails service addBulkSiteExpenseDetails: ',
      error
    );
    throw error;
  }
};

export { createSiteExpenseDetails, addBulkSiteExpenseDetails };
