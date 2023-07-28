import hsnCodeService from '../../service/hsnCode-service';

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
      .max(15, masterErrorMessages.MAX_CODE),
    //   .test(
    //     'code-availability',
    //     masterErrorMessages.CODE_EXIST,
    //     async (value: any) => {
    //       if (value) {
    //         const response = await hsnCodeService.getByHsnCode(value);
    //         if (response?.success === true) {
    //           return false;
    //         } else {
    //           return true;
    //         }
    //       }
    //     }
    //   ),
    master_data_description: yup
      .string()
      .typeError(masterErrorMessages.ENTER_DESCRIPTION)
      .required(masterErrorMessages.ENTER_DESCRIPTION),
  });
};
// export const getUpdateValidateyup = (yup: any) => {
//   return yup.object().shape({
//     hsn_code_id: yup.number().required(),
//     code: yup
//       .string()
//       .typeError(masterErrorMessages.ENTER_CODE)
//       .required(masterErrorMessages.ENTER_CODE)
//       .matches(/^[0-9]+$/, masterErrorMessages.ENTER_NUMBERONLY)
//       .min(4, masterErrorMessages.MIN_CODE)
//       .max(8, masterErrorMessages.MAX_CODE)
//       .test(
//         'code-availability',
//         masterErrorMessages.CODE_EXIST,
//         async (value: any, { parent }: yup.TestContext) => {
//           const hsnCode = parent.hsn_code_id;
//           if (value) {
//             const response = await hsnCodeService.getByHsnCode(value);
//             if (
//               response?.success === true &&
//               response.data.hsn_code_id === hsnCode
//             ) {
//               return true;
//             } else {
//               return false;
//             }
//           }
//         }
//       ),
//     description: yup
//       .string()
//       .typeError(masterErrorMessages.ENTER_DESCRIPTION)
//       .required(masterErrorMessages.ENTER_DESCRIPTION),
//   });
// };
