  export const IndentErrorMessages = {
    ENTER_COMMENTS: 'Rejection comments required',
  };
  
  export const getIndentRejectValidateyup = (yup: any) => {
    return yup.object().shape({
        approver_comments: yup.string().trim().required(IndentErrorMessages.ENTER_COMMENTS),
    });
  };
  