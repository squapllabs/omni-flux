

export const BOQErrorMessages = {
  ENTER_NAME: 'BOQ name is required',
  ENTER_DESCRIPTION: 'BOQ description is required',
  ENTER_TYPE: 'BoQ Type is required'
};


export const getCreateBoQValidateyup = (yup: any) => {
  return yup.object().shape({
    // bom_name: yup.string().required(BOQErrorMessages.ENTER_NAME),
    bom_description: yup.string().trim().required(BOQErrorMessages.ENTER_DESCRIPTION),
    bom_type_id: yup
      .string()
      .required(BOQErrorMessages.ENTER_TYPE),
    //   .test(
    //     'decimal-validation',
    //     'Already exist',
    //     // async function (value: number, { parent }: yup.TestContext) {
    //     //   let isDelete = parent.is_delete;
    //     //   try {
    //     //     const isValuePresent = bomConfig.some((obj: any) => {
    //     //       return (
    //     //         Number(obj.bom_type_id) === Number(value) &&
    //     //         obj.is_delete === isDelete
    //     //       );
    //     //     });
    //     //     if (isValuePresent === false) {
    //     //       return true;
    //     //     } else return false;
    //     //   } catch {
    //     //     return true;
    //     //   }
    //     // }
    //   ),
  });
};



