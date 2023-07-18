import hsnCodeService from '../../service/hsnCode-service';

export const userErrorMessages = {
  ENTER_CODE: 'Code is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_NUMBERONLY: 'Number only allowed',
  MIN_CODE: 'Minimum 4 digit needed',
  MAX_CODE: 'Maximum 8 digit needed',
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
        'Code is already present',
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
        'Code is already present',
        async (value: any, { parent }: yup.TestContext) => {
          const hsnCode = parent.hsn_code_id;
          console.log('hsnCode', hsnCode);
          if (!hsnCode) {
            if (value) {
              const response = await hsnCodeService.getByHsnCode(value);
              if (response?.success === true) {
                return false;
              } else {
                return true;
              }
            }
            return false;
          }
          return true;
        }
      ),
    description: yup
      .string()
      .typeError(userErrorMessages.ENTER_DESCRIPTION)
      .required(userErrorMessages.ENTER_DESCRIPTION),
  });
};
