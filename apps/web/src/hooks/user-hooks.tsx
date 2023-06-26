import { useQuery } from "react-query";
import userService from "../service/user-service";

const useGetAllUsers = () => {
    return useQuery(["useGetAllUsers"],() => userService.getAllUsers(),
      {
        select: (data) => data.data,
      }
    );
  };
  
export { useGetAllUsers };