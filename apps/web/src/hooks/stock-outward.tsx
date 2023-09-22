import { useQuery, useMutation, useQueryClient } from 'react-query';
import stockOutwardService from '../service/stock-outward-service';


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