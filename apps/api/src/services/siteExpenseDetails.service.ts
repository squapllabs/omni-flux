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
      status,
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
export { createSiteExpenseDetails };
