import uomService from '../../service/uom-service';

export const userErrorMessages = {
  ENTER_NAME: 'Unit Of Measurement is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_SPECIAL_CHARACTER: 'Special Characters are not allowed',
  NAME_EXIST: 'Unit of Measurement is already present',
};

export const getuomCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .matches(/^[a-zA-Z0-9\s]+$/, userErrorMessages.ENTER_SPECIAL_CHARACTER)
      .test(
        'uom-availability',
        userErrorMessages.NAME_EXIST,
        async (value: any) => {
          if (value) {
            const response = await uomService.getOneUomByName(value);
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
export const getuomUpdateValidateyup = (yup: any) => {
  return yup.object().shape({
    uom_id: yup.number().required(),
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .matches(/^[a-zA-Z0-9]+$/, userErrorMessages.ENTER_SPECIAL_CHARACTER)
      .test(
        'uom-availability',
        userErrorMessages.NAME_EXIST,
        async (value: any, { parent }: yup.TestContext) => {
          const uomCode = parent.uom_id;
          if (value) {
            const response = await uomService.getOneUomByName(value);
            if (
              response?.success === true &&
              response.data[0].uom_id === uomCode
            ) {
              return true;
            } else {
              return false;
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
