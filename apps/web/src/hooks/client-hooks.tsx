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

const useGetByuserID = (id: number) => {
  return useQuery(
    ['getOneClientyID', id],
    () => ClientService.getOneClientByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const useCreateClient = () => {
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

const useInstantcreateClient = () => {
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

const useUpdateClient = () => {
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
const useGetByClient = () => {
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
  useGetByuserID,
  useCreateClient,
  useUpdateClient,
  useDeleteClient,
  useGetAllClientDrop,
  useGetByClient,
  useInstantcreateClient,
  useGetAllPaginatedClient,
};
