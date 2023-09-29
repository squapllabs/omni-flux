import { useQuery, useMutation, useQueryClient } from 'react-query';
import vendorQuotesService from '../service/vendorQuotes-service';

const updateVendorQuotes = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return vendorQuotesService.updateVendorQuotes(data);
      },
      {
        onSuccess: (data, _v) => {
          queryClient.invalidateQueries(['getSubcategoryList'], _v.category_id);
        },
      }
    );
  };

  const getByQuoteVendorId = (id: number) => {
    return useQuery(
      ['getByuserID', id],
      () => vendorQuotesService.getOneVendorQuotesById(id),
      {
        select: (data) => data.data,
      }
    );
  };
  export {updateVendorQuotes,getByQuoteVendorId};