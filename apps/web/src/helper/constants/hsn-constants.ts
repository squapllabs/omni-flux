import hsnCodeService from '../../service/hsnCode-service';

export const userErrorMessages = {
  ENTER_CODE: 'HSN Code is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_NUMBERONLY: 'Number only allowed',
  MIN_CODE: 'Minimum 4 digit needed',
  MAX_CODE: 'Maximum 8 digit needed',
  CODE_EXIST: 'Entered HSN Code is already present',
};

export const gethsnCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    code: yup
      .string()
      .typeError(userErrorMessages.ENTER_CODE)
      .required(userErrorMessages.ENTER_CODE)
      .matches(/^[0-9]+$/, userErrorMessages.ENTER_NUMBERONLY)
      .min(4, userErrorMessages.MIN_CODE)
      .max(8, userErrorMessages.MAX_CODE)
      .test(
        'code-availability',
        userErrorMessages.CODE_EXIST,
        async (value: any) => {
          if (value) {
            const response = await hsnCodeService.getByHsnCode(value);
            if (response?.success === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    description: yup
      .string()
      .typeError(userErrorMessages.ENTER_DESCRIPTION)
      .required(userErrorMessages.ENTER_DESCRIPTION),
  });
};
export const gethsnUpdateValidateyup = (yup: any) => {
  return yup.object().shape({
    hsn_code_id: yup.number().required(),
    code: yup
      .string()
      .typeError(userErrorMessages.ENTER_CODE)
      .required(userErrorMessages.ENTER_CODE)
      .matches(/^[0-9]+$/, userErrorMessages.ENTER_NUMBERONLY)
      .min(4, userErrorMessages.MIN_CODE)
      .max(8, userErrorMessages.MAX_CODE)
      .test(
        'code-availability',
        userErrorMessages.CODE_EXIST,
        async (value: any, { parent }: yup.TestContext) => {
          const hsnCode = parent.hsn_code_id;
          if (value) {
            const response = await hsnCodeService.getByHsnCode(value);
            if (
              response?.success === true &&
              response.data.hsn_code_id === hsnCode
            ) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    description: yup
      .string()
      .typeError(userErrorMessages.ENTER_DESCRIPTION)
      .required(userErrorMessages.ENTER_DESCRIPTION),
  });
};
