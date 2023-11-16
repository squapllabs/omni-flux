import { useQuery, useMutation, useQueryClient } from 'react-query';
import indentApprovalService from '../service/indent-approval-request-service';

const useGetByUserRoleIndent = () => {
  return useMutation((data: any) => {
    return indentApprovalService.indentRaise(data);
  });
};

const useGetAllIndentbyUserRole = (data: any) => {
  return useQuery(
    ['getAllIndentbyUserRole'],
    () => indentApprovalService.indentRaise(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useGetAllIndentRequestDetail = (data: any) => {
  return useQuery(
    ['useGetAllIndentRequestData'],
    () => indentApprovalService.indentDetailData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useUpdateIndentRequest = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return indentApprovalService.updateIndentRequest(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllClientData']);
      },
    }
  );
};

const useGetByIndnetId = (id: number) => {
  return useQuery(
    ['getByIndnetId', id],
    () => indentApprovalService.getOneIndentById(id),
    {
      select: (data) => data.data,
    }
  );
};

export {
  useGetByUserRoleIndent,
  useGetAllIndentRequestDetail,
  useUpdateIndentRequest,
  useGetAllIndentbyUserRole,
  useGetByIndnetId,
};
