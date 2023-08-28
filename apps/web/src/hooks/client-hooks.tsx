import { useQuery, useMutation, useQueryClient } from 'react-query';
import ClientService from '../service/client-service';

const useGetAllClient = () => {
  return useQuery(['useGetAllClient'], () => ClientService.getAllClient(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useGetAllClientDrop = () => {
  return useQuery(['useGetAllClientDrop'], () => ClientService.getAllClient(), {
    select: (data) =>
      data?.data?.map((client: any) => ({
        value: client.client_id,
        label: client.name,
      })),
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
        queryClient.invalidateQueries(['useGetAllClientData']);
      },
    }
  );
};

const instantcreateClient = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ClientService.createClient(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllClientDrop']);
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
        queryClient.invalidateQueries(['useGetAllClientData']);
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
        queryClient.invalidateQueries(['useGetAllClientData']);
      },
    }
  );
};
const getByClient = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ClientService.filterClient(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useGetAllPaginatedClient = (data: any) => {
  return useQuery(
    ['useGetAllClientData'],
    () => ClientService.filterClient(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

export {
  useGetAllClient,
  getByuserID,
  createClient,
  updateClient,
  useDeleteClient,
  useGetAllClientDrop,
  getByClient,
  instantcreateClient,
  useGetAllPaginatedClient,
};
