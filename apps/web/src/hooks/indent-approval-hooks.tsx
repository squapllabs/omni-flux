import { useQuery, useMutation, useQueryClient } from 'react-query';
import indentApprovalService from '../service/indent-approval-request-service';


const getByUserRoleIndent = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return indentApprovalService.indentRaise(data);
      },
      {
        onSuccess: (response) => {
          response;
        },
      }
    );
  };


  export { getByUserRoleIndent};
