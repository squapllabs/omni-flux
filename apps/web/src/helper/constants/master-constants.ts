import hsnCodeService from '../../service/hsnCode-service';
import masterDataService from '../../service/masterData-service';

export const masterErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_CODE: 'Code is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_NUMBERONLY: 'Number only allowed',
  MIN_CODE: 'Code must be more then 3',
  MAX_CODE: 'Code must lesser then 15',
  CODE_EXIST: 'Code is already present',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    master_data_name: yup
      .string()
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
        async (value: any, { parent }: yup.TestContext) => {
          const id = parent.parent_master_data_id;
          console.log('id', id);
          let object: any = {
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
      .typeError(masterErrorMessages.ENTER_DESCRIPTION)
      .required(masterErrorMessages.ENTER_DESCRIPTION),
    master_data_name: yup
      .string()
      .typeError(masterErrorMessages.ENTER_NAME)
      .required(masterErrorMessages.ENTER_NAME),
  });
};
