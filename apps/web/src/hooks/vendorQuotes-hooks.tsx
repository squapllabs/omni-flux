import { useQuery, useMutation, useQueryClient } from 'react-query';
import vendorQuotesService from '../service/vendorQuotes-service';

const useUpdateVendorQuotes = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return vendorQuotesService.updateVendorQuotes(data);
    },
    {
      onSuccess: (data, _v) => {
        queryClient.invalidateQueries(['getSubcategoryList'], _v.category_id);
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const useGetByQuoteVendorId = (id: number) => {
  return useQuery(
    ['getByuserID', id],
    () => vendorQuotesService.getOneVendorQuotesById(id),
    {
      select: (data) => data.data,
    }
  );
};
const useGetVendorQuotesbyPRIDDrop = (id: number) => {
  return useQuery(
    ['getVendorQuotesbyPRID', id],
    () => vendorQuotesService.getVendorQuotesBasedONPR(id),
    {
      select: (data) => data.data,
    }
  );
};
const useGetVendorDetailsBasedONPR = (id: number) => {
  return useQuery(
    ['useGetVendorDetailsBasedONPR', id],
    () => vendorQuotesService.getVendorDetailsBasedONPR(id),
    {
      select: (data) =>
        data?.data?.map((data: any) => ({
          value: data.vendor_quotes_id,
          label: data.vendor_name,
          data: data,
        })),
    }
  );
};

const useGetByPRbasedVendorQuotes = (data: any) => {
  return useQuery(
    ['useGetByPRbasedVendorQuotes'],
    () => vendorQuotesService.vendorQuotesData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};
const useGetVendorquotesBYventorquotesID = (data: any) => {
  return useQuery(
    ['useGetVendorquotesBYventorquotesID'],
    () => vendorQuotesService.getVendorquotesBYventorquotesID(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};
export {
  useUpdateVendorQuotes,
  useGetByQuoteVendorId,
  useGetByPRbasedVendorQuotes,
  useGetVendorQuotesbyPRIDDrop,
  useGetVendorDetailsBasedONPR,
  useGetVendorquotesBYventorquotesID,
};
