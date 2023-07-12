import { useQuery,useMutation,useQueryClient } from 'react-query';
import userService from '../service/user-service';

const useGetAllUsers = () => {
  return useQuery(['useGetAllUsers'], () => userService.getAllUsers(), {
    select: (data) => data.data,
  });
};

const getByloginID = (id: string) => {
  return useQuery(['getByLoginID', id], () => userService.getOneUser(id));
};
const getByuserID = (id: number) => {
  return useQuery(['getByuserID', id], () => userService.getOneUserbyID(id),{
    select: (data) => data.data,
  });
};

const useDeleteUsers = () => {
  const queryClient = useQueryClient();
  return useMutation(
      (data : any) => {
          return userService.deleteUser(data) ;
      },
      {
          onSuccess: () => {
              queryClient.invalidateQueries(["useGetAllUsers"]);
          },
      }
  );
};

export { useGetAllUsers, getByloginID, getByuserID,useDeleteUsers };
