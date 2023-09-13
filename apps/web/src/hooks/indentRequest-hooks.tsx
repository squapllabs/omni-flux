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

export { getProjectBasedIndent, createIndentRequest };
