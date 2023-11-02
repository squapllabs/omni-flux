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

const getAllIndentbyUserRole = (data: any) => {
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

const updateIndentRequest = () => {
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

const getByIndnetId = (id: number) => {
  return useQuery(
    ['getByIndnetId', id],
    () => indentApprovalService.getOneIndentById(id),
    {
      select: (data) => data.data,
    }
  );
};

export {
  getByUserRoleIndent,
  useGetAllIndentRequestDetail,
  updateIndentRequest,
  getAllIndentbyUserRole,
  getByIndnetId
};
