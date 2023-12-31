import masterDataService from '../../service/masterData-service';

export const masterErrorMessages = {
  ENTER_NAME: 'Name is required',
  MAX_NAME: 'Name must be lesser then 20 characters',
  MIN_NAME: 'Name must be greater then 3 characters',
  ENTER_CODE: 'Code is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_NUMBERONLY: 'Number only allowed',
  MIN_CODE: 'Code must be more then 3 letters',
  MAX_CODE: 'Code must lesser then 15',
  CODE_EXIST: 'Code is already present',
};

export const getCreateValidateyup = (yup: any,projectId:string) => {
  return yup.object().shape({
    master_data_name: yup
      .string()
      .max(20, masterErrorMessages.MAX_NAME)
      .min(3, masterErrorMessages.MIN_NAME)
      .typeError(masterErrorMessages.ENTER_NAME)
      .required(masterErrorMessages.ENTER_NAME),
    master_data_type: yup
      .string()
      .typeError(masterErrorMessages.ENTER_CODE)
      .required(masterErrorMessages.ENTER_CODE)
      .min(3, masterErrorMessages.MIN_CODE)
      .max(15, masterErrorMessages.MAX_CODE)
      .test(
        'code-availability',
        masterErrorMessages.CODE_EXIST,
        async (value: any,  { parent } : yup.TestContext) => {
          const id = parent.parent_master_data_id;
          const object: any = {
            name: value,
            id: id === undefined ? null : Number(id),
            project_id :projectId === undefined ? null : Number(projectId)
          };
          if (value) {
            const response = await masterDataService.checkDublicatemasertData(
              object
            );
            if (response?.status === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    master_data_description: yup
      .string()
      .trim()
      .typeError(masterErrorMessages.ENTER_DESCRIPTION)
      .required(masterErrorMessages.ENTER_DESCRIPTION),
    parent_master_data_id: yup.string().typeError().notRequired(),
  });
};
export const getMasterCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    master_data_name: yup
      .string()
      .max(20, masterErrorMessages.MAX_NAME)
      .min(3, masterErrorMessages.MIN_NAME)
      .typeError(masterErrorMessages.ENTER_NAME)
      .required(masterErrorMessages.ENTER_NAME),
    master_data_type: yup
      .string()
      .typeError(masterErrorMessages.ENTER_CODE)
      .required(masterErrorMessages.ENTER_CODE)
      .min(3, masterErrorMessages.MIN_CODE)
      .max(15, masterErrorMessages.MAX_CODE)
      .test(
        'code-availability',
        masterErrorMessages.CODE_EXIST,
        async (value: any,  { parent } : yup.TestContext) => {
          const id = parent.parent_master_data_id;
          const object: any = {
            name: value,
            id: id === undefined ? null : Number(id),
          };
          if (value) {
            const response = await masterDataService.checkDublicatemasertData(
              object
            );
            if (response?.status === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    master_data_description: yup
      .string()
      .trim()
      .typeError(masterErrorMessages.ENTER_DESCRIPTION)
      .required(masterErrorMessages.ENTER_DESCRIPTION),
    parent_master_data_id: yup.string().typeError().notRequired(),
  });
};
export const getUpdateValidateyup = (yup: any) => {
  return yup.object().shape({
    master_data_id: yup.string().trim().required(),
    master_data_description: yup
      .string()
      .trim()
      .typeError(masterErrorMessages.ENTER_DESCRIPTION)
      .required(masterErrorMessages.ENTER_DESCRIPTION),
    master_data_name: yup
      .string()
      .max(20, masterErrorMessages.MAX_NAME)
      .min(3, masterErrorMessages.MIN_NAME)
      .typeError(masterErrorMessages.ENTER_NAME)
      .required(masterErrorMessages.ENTER_NAME),
  });
};
