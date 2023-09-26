import ProjectSettingsService from "../../service/projectSettings-service";
import MasterDataService from "../../service/masterData-service";

export const projectMemberErrorMessages = {
    ENTER_ROLE: 'Project Role is required',
    ENTER_USER: 'Project Member is required',
    USER_EXIST: 'This Project and User combination already exist'
};

export const masterErrorMessages = {
    ENTER_NAME: 'Name is required',
    ENTER_CODE: 'Code is required',
    ENTER_DESCRIPTION: 'Description is required',
    ENTER_NUMBERONLY: 'Number only allowed',
    MIN_CODE: 'Code must be more then 3',
    MAX_CODE: 'Code must lesser then 15',
    CODE_EXIST: 'Code is already present',
};

export const getProjectMemberCreationYupschema = (yup: any) => {
    return yup.object().shape({
        project_id: yup.number(),
        project_role_id: yup
            .number()
            .required(projectMemberErrorMessages.ENTER_ROLE),
        user_id: yup
            .number()
            .required(projectMemberErrorMessages.ENTER_USER)
            .test('code-availability',
                projectMemberErrorMessages.USER_EXIST,
                async (value: string | number | Date, { parent }: yup.TestContext) => {
                    if (value) {
                        const projectId = parent.project_id;
                        const response = await ProjectSettingsService.fetchProjectUser(value, projectId);
                        if (response?.is_exist === true) {
                            return false;
                        } else {
                            return true;
                        }
                    }
                }
            ),
        access_start_date: yup.date(),
        access_end_date: yup
            .date()
            .min(yup.ref('access_start_date'), 'End date cannot be earlier than start date')
            .test(
                'is-greater',
                'End date must be greater than the start date',
                function (value: string | number | Date, { parent }: yup.TestContext) {
                    const startDate = parent.access_start_date;
                    if (!startDate || !value) return true;
                    return new Date(value) > new Date(startDate);
                }
            ),
    });
};

export const getCreateMasterValidateyup = (yup: any) => {
    return yup.object().shape({
        project_id: yup.number(),
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
                    const id = parent.project_id;
                    const object: any = {
                        type: value,
                        id: id === undefined ? null : Number(id),
                    };
                    console.log("obje",object);
                    
                    if (value) {
                        const response = await MasterDataService.checkDublicateProjectMasertData(
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
    });
};