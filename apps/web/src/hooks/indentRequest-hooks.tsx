import { useQuery, useMutation, useQueryClient } from 'react-query';
import IndentService from '../service/indentRequest-service';

const useGetProjectBasedIndent = (id: number) => {
  return useQuery(
    ['getProjectBasedIndent', id],
    () => IndentService.getIndentByProjectID(id),
    {
      select: (data) => data.data,
    }
  );
};

const useCreateIndentRequest = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return IndentService.createIndentRequest(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getProjectBasedIndent']);
      },
    }
  );
};
const useUpdateIndentRequest = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return IndentService.updateIndentRequest(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getProjectBasedIndent']);
      },
    }
  );
};

const useGetBySearchIndent = () => {
  return useMutation(
    (data: any) => {
      return IndentService.filterIndnet(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useGetIndentSearchPaginated = (data : any) =>{
  return useQuery(
    ['getIndentSearchPaginated'],
    () => IndentService.filterIndnet(data),
    {
      // select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

export {
  useGetProjectBasedIndent,
  useCreateIndentRequest,
  useUpdateIndentRequest,
  useGetBySearchIndent,
  useGetIndentSearchPaginated
};
