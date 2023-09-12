
export const projectMemberErrorMessages = {
    ENTER_ROLE: 'Project Role is required',
    ENTER_USER: 'Project Member is required',

};

export const getProjectMemberCreationYupschema = (yup: any) => {
    return yup.object().shape({
        project_role_id: yup
            .number()
            .required(projectMemberErrorMessages.ENTER_ROLE),
        user_id: yup
            .number()
            .required(projectMemberErrorMessages.ENTER_USER),
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