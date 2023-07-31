import { useQuery, useMutation, useQueryClient } from 'react-query';
import userService from '../service/user-service';

const useGetAllUsers = () => {
  return useQuery(['useGetAllUsers'], () => userService.getAllUsers(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useGetAllInactiveUsers = () => {
  return useQuery(
    ['useGetAllInactiveUsers'],
    () => userService.getAllInactiveUsers(),
    {
      select: (data) => data.data,
    }
  );
};

const getByloginID = (id: string) => {
  return useQuery(['getByLoginID', id], () => userService.getOneUser(id));
};
const getByuserID = (id: number) => {
  return useQuery(['getByuserID', id], () => userService.getOneUserbyID(id), {
    select: (data) => data.data,
  });
};
const createUser = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return userService.createuser(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUsers']);
      },
    }
  );
};

const updateUser = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return userService.updateUser(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUsers']);
      },
    }
  );
};

const useDeleteUsers = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return userService.deleteUser(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUsers']);
      },
    }
  );
};
export {
  useGetAllUsers,
  getByloginID,
  getByuserID,
  createUser,
  updateUser,
  useDeleteUsers,
  useGetAllInactiveUsers,
};
