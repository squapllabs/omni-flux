import { useQuery, useMutation, useQueryClient } from 'react-query';
import IndentService from '../service/indentRequest-service';

const getProjectBasedIndent = (id: number) => {
  return useQuery(
    ['getProjectBasedIndent', id],
    () => IndentService.getIndentByProjectID(id),
    {
      select: (data) => data.data,
    }
  );
};

const createIndentRequest = () => {
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
const updateIndentRequest = () => {
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

const getBySearchIndent = () => {
  const queryClient = useQueryClient();
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

const getIndentSearchPaginated = (data : any) =>{
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
  getProjectBasedIndent,
  createIndentRequest,
  updateIndentRequest,
  getBySearchIndent,
  getIndentSearchPaginated
};
