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
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
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
const getVendorQuotesbyPRIDDrop = (id: number) => {
  return useQuery(
    ['getVendorQuotesbyPRID', id],
    () => vendorQuotesService.getVendorQuotesBasedONPR(id),
    {
      select: (data) => data.data,
    }
  );
};
const getVendorDetailsBasedONPR = (id: number) => {
  return useQuery(
    ['getVendorDetailsBasedONPR', id],
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

const getByPRbasedVendorQuotes = (data: any) => {
  return useQuery(
    ['getByPRbasedVendorQuotes'],
    () => vendorQuotesService.vendorQuotesData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};
const getVendorquotesBYventorquotesID = (data: any) => {
  return useQuery(
    ['getVendorquotesBYventorquotesID'],
    () => vendorQuotesService.getVendorquotesBYventorquotesID(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};
export {
  updateVendorQuotes,
  getByQuoteVendorId,
  getByPRbasedVendorQuotes,
  getVendorQuotesbyPRIDDrop,
  getVendorDetailsBasedONPR,
  getVendorquotesBYventorquotesID,
};
