import { useQuery, useQueryClient, useMutation } from "react-query";
import userService from "../service/user-service";

const useGetAllUsers = () => {
  return useQuery(["useGetAllUsers"], () => userService.getAllUsers(),
    {
      select: (data) => data.data,
    }
  );
};


const getByloginID = (id: any) => {
  return useQuery(["getByLoginID", id], () => userService.getOneUser(id)
  )
}

 const loginAuth = () => {
  const queryClient = useQueryClient();
  // return useMutation(
  //   (data) => {
  //     return userService.loginAuth(data);
  //   },
  //   onSuccess: async () => {
  //     console.log("I'm first!")
  //   },
  //   onSettled: async () => {
  //     console.log("I'm second!")
  //   },
    
  // )

  return useMutation({
    mutationFn: userService.loginAuth
  })
}
export { useGetAllUsers, loginAuth, getByloginID };