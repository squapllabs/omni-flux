import { useQuery, useMutation, useQueryClient } from 'react-query';
import siteExpenseService from '../service/expense-service';

const useGetAllsiteExpense = () => {
  return useQuery(
    ['useGetAllsiteExpense'],
    () => siteExpenseService.getAllsiteExpense(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const getBysiteExpenseID = (id: number) => {
  return useQuery(
    ['getOnesiteExpenseID', id],
    () => siteExpenseService.getOnesiteExpenseByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const createsiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.createsiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const updatesiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.updatesiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const updatesiteExpenseDetail = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.updatesiteExpenseDetail(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const useDeletesiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.deletesiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const getBySearchsiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.filtersiteExpense(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};
const useBulkuploadSiteExpanse = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.createsiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};
export {
  useGetAllsiteExpense,
  getBysiteExpenseID,
  createsiteExpense,
  updatesiteExpense,
  useDeletesiteExpense,
  getBySearchsiteExpense,
  useBulkuploadSiteExpanse,
  updatesiteExpenseDetail,
};
