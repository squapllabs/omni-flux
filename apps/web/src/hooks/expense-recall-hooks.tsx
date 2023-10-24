import { useMutation, useQueryClient } from 'react-query';
import expenseRecallService from '../service/expense-recall-service';

const createExpenseRecall = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return expenseRecallService.createSiteExpenseRecall(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['']);
      },
    }
  );
};

export { createExpenseRecall };
