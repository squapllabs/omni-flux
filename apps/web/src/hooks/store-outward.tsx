import { useQuery, useMutation, useQueryClient } from 'react-query';
import stockOutwardService from '../service/store-outward-service';


const getAllStockOutwardData = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return stockOutwardService.getStockOutWardData(data);
      },
      {
        onSuccess: (response) => {
          response;
        },
      }
    );
  };

  export {getAllStockOutwardData}