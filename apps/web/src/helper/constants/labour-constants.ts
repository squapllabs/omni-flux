import LabourService from "../../service/labour-service";

export const labourErrorMessages = {
    ENTER_TYPE: 'Labour Type is required',
    ENTER_UOMTYPE: 'UOM Type is required',
    ENTER_RATE: 'Rate is required',
    TYPE_ERROR: 'Only Number are allowed',
    DECIMAL_CHECK: 'Decimal values are not allowed',
    ENTER_VALID_RATE: 'Check your values',
    TYPE_EXIST: 'Entered Labour Type is already present',
};

export const getLabourCreationYupschema = (yup: any) => {
    return yup.object().shape({
        labour_type: yup
            .string()
            .trim()
            .required(labourErrorMessages.ENTER_TYPE)
            .test('code-availability',
                labourErrorMessages.TYPE_EXIST,
                async (value: any) => {
                    if (value) {
                        const response = await LabourService.getByLabourType(value);
                        if (response?.is_exist === true) {
                            return false;
                        } else {
                            return true;
                        }
                    }
                }
            ),
        uom_id: yup
            .number()
            .required(labourErrorMessages.ENTER_UOMTYPE),
        rate: yup
            .number()
            .typeError(labourErrorMessages.TYPE_ERROR)
            .required(labourErrorMessages.ENTER_RATE),
    });
};

export const getLabourUpdateYupschema = (yup: any) => {
    return yup.object().shape({
        labour_id: yup.number().required(),
        labour_type: yup
            .string()
            .trim()
            .required(labourErrorMessages.ENTER_TYPE)
            .test('code-availability',
                labourErrorMessages.TYPE_EXIST,
                async (value: any, { parent }: yup.TestContext) => {
                    const labourId = parent.labour_id;
                    if (value) {
                        const response = await LabourService.getByLabourType(value);
                        if (response?.is_exist === true && response?.data?.labour_id === labourId) {
                            return true;
                        } else {
                            return false;
                        }
                    }
                }
            ),
        uom_id: yup
            .number()
            .required(labourErrorMessages.ENTER_UOMTYPE),
        rate: yup
            .number()
            .typeError(labourErrorMessages.TYPE_ERROR)
            .required(labourErrorMessages.ENTER_RATE),
    });
};
