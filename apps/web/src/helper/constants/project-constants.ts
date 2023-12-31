import projectService from '../../service/project-service';
export const ProjectMessages = {
  ENTER_NAME: 'Project name is required',
  ENTER_CODE: 'Project code is required',
  ENTER_USER: 'Project manager is required',
  ENTER_CLIENT: 'Project client/customer is required',
  MINIMUM_CHECK: 'Value must be greater than 0',
  MAXIMUM_CHECK: 'Value must be less then 100000',
  TYPE_ERROR: 'Only Number are allowed',
  TYPE_ACTUAL: 'Actual budget is required',
  TYPE_ESTIMATE: 'Estimated budget is required',
  CODE_EXIST: 'Code is already present',
  ENTER_PRIORITY: 'Priority is required',
  SELECT_START_DATE: 'Project start date is required',
  SELECT_END_DATE: 'Project end date is required',
  SELECT_SITE: 'Site is required',
  ENTER_APPROVAR: 'Approver is required',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    project_name: yup.string().required(ProjectMessages.ENTER_NAME),
    code: yup
      .string()
      .required(ProjectMessages.ENTER_CODE)
      .test(
        'code-availability',
        ProjectMessages.CODE_EXIST,
        async (value: any) => {
          if (value) {
            const response = await projectService.checkProjectCodeDuplicate(
              value
            );
            if (response?.is_exist === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    user_id: yup.string().trim().required(ProjectMessages.ENTER_USER),
    approvar_id: yup.string().trim().required(ProjectMessages.ENTER_APPROVAR),
    client_id: yup.string().trim().required(ProjectMessages.ENTER_CLIENT),
    estimated_budget: yup
      .number()
      .min(1, ProjectMessages.MINIMUM_CHECK)
      .max(100000, ProjectMessages.MAXIMUM_CHECK)
      .typeError(ProjectMessages.TYPE_ERROR)
      .required(ProjectMessages.TYPE_ESTIMATE),
    actual_budget: yup
      .number()
      .min(1, ProjectMessages.MINIMUM_CHECK)
      .max(100000, ProjectMessages.MAXIMUM_CHECK)
      .typeError(ProjectMessages.TYPE_ERROR)
      .required(ProjectMessages.TYPE_ACTUAL),
    priority: yup.string().trim().required(ProjectMessages.ENTER_PRIORITY),
    date_started: yup.date().required(ProjectMessages.SELECT_START_DATE),
    date_ended: yup
      .date()
      .required(ProjectMessages.SELECT_END_DATE)
      .min(
        yup.ref('date_started'),
        'Project end date cannot be earlier than start date'
      ),
    site_configuration: yup
      .array()
      .test(
        'unique-site-ids',
        'Site name repeated are not allowed',
        function (sites: any) {
          const siteIds = new Set();
          for (const site of sites) {
            if (siteIds.has(site.site_id)) {
              return false; // Duplicate site_id found
            }
            siteIds.add(site.site_id);
          }
          return true; // No duplicate site_id found
        }
      )
      .test(
        'non-empty-array',
        ProjectMessages.SELECT_SITE,
        function (sites: any) {
          return sites.length > 0; // Check if array is not empty
        }
      )
      .of(
        yup.object().shape({
          approvar_id: yup.string().trim().required('Approver is required'), // Validate approvar_id
          estimation: yup
            .number()
            .required('Estimation is required')
            .typeError('Only numbers are allowed')
            .test(
              'site-budget',
              'Site budget is greater than estimated budget',
              function (estimation: any, { parent }: yup.TestContext) {
                const estimated_budget = parent;
                console.log('estimated_budget =>', estimated_budget);
                console.log('estimation -->', estimation);
                console.log('estimation type =>', typeof estimation);
                if (estimation > estimated_budget) {
                  return false; // Site budget is greater than estimated budget
                }
                return true;
              }
            ),
        })
      ),
  });
};

export const getEditValidateyup = (yup: any) => {
  return yup.object().shape({
    project_name: yup.string().required(ProjectMessages.ENTER_NAME),
    user_id: yup.string().trim().required(ProjectMessages.ENTER_USER),
    client_id: yup.string().trim().required(ProjectMessages.ENTER_CLIENT),
    estimated_budget: yup
      .number()
      .min(1, ProjectMessages.MINIMUM_CHECK)
      .max(100000, ProjectMessages.MAXIMUM_CHECK)
      .typeError(ProjectMessages.TYPE_ERROR)
      .required(ProjectMessages.TYPE_ESTIMATE),
    actual_budget: yup
      .number()
      .min(1, ProjectMessages.MINIMUM_CHECK)
      .max(100000, ProjectMessages.MAXIMUM_CHECK)
      .typeError(ProjectMessages.TYPE_ERROR)
      .required(ProjectMessages.TYPE_ACTUAL),
    priority: yup.string().trim().required(ProjectMessages.ENTER_PRIORITY),
    date_started: yup.date().required(ProjectMessages.SELECT_START_DATE),
    date_ended: yup
      .date()
      .required(ProjectMessages.SELECT_END_DATE)
      .min(
        yup.ref('date_started'),
        'Project end date cannot be earlier than start date'
      ),
    site_configuration: yup
      .array()
      .test(
        'unique-site-ids',
        'Site name repeated are not allowed',
        function (sites: any) {
          const siteIds = new Set();
          for (const site of sites) {
            if (siteIds.has(site.site_id)) {
              return false; // Duplicate site_id found
            }
            siteIds.add(site.site_id);
          }
          return true; // No duplicate site_id found
        }
      ),
  });
};

export const getProjectStockAuditValidate = (yup: any) => {
  return yup.object().shape({
    site_id: yup.string().trim().required(ProjectMessages.SELECT_SITE),
    // site_audit_date: yup.date().required('Site Audit Date id required'),
  });
};
