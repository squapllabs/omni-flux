import uomService from '../../service/uom-service';

export const userErrorMessages = {
  ENTER_NAME: 'Unit Of Measurement is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_SPECIAL_CHARACTER: 'Special Characters are not allowed',
};

export const getuomCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .matches(/^[a-zA-Z0-9]+$/, userErrorMessages.ENTER_SPECIAL_CHARACTER)
      .test(
        'uom-availability',
        'Unit of Measurement is already present',
        async (value: any) => {
          if (value) {
            const response = await uomService.getOneUomByName(value);
            console.log('response', response);

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
        'Unit of Measurement is already present',
        async (value: any, { parent }: yup.TestContext) => {
          const uomCode = parent.uom_id;
          console.log('hsnCode', uomCode);
          if (!uomCode) {
            if (value) {
              const response = await uomService.getOneUomByName(value);
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
