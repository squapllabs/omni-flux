import { useQuery, useMutation, useQueryClient } from 'react-query';
import BomService from '../service/bom-service';

const createAbstract = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return BomService.createBomData(data);
      },
      {
        onSuccess: (response) => {
            response
        //   queryClient.invalidateQueries(['useGetAllClientData']);
        },
      }
    );
  };
  
  export {
    createAbstract
  }