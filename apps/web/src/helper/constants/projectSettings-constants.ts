import ProjectSettingsService from "../../service/projectSettings-service";

export const projectMemberErrorMessages = {
    ENTER_ROLE: 'Project Role is required',
    ENTER_USER: 'Project Member is required',
    USER_EXIST: 'This Project and User combination already exist'
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
                 async(value: string | number | Date, { parent }: yup.TestContext) => {
                    if (value ) {
                        const projectId = parent.project_id;
                        const response = await ProjectSettingsService.fetchProjectUser(value,projectId);                        
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