import { useQuery, useMutation, useQueryClient } from 'react-query';
import ClientService from '../service/client-service';

const useGetAllClient = () => {
  return useQuery(['useGetAllClient'], () => ClientService.getAllClient(), {
    select: (data) => data.data,
  });
};

const getByuserID = (id: number) => {
  return useQuery(
    ['getOneClientyID', id],
    () => ClientService.getOneClientByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const createClient = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ClientService.createClient(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllClient']);
      },
    }
  );
};

const updateClient = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ClientService.updateClient(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllClient']);
      },
    }
  );
};

const useDeleteClient = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ClientService.deleteClient(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllClient']);
      },
    }
  );
};

export {
  useGetAllClient,
  getByuserID,
  createClient,
  updateClient,
  useDeleteClient,
};
