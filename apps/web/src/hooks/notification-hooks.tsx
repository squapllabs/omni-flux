import { useMutation, useQuery, useQueryClient } from 'react-query';
import notificationService from '../service/notification-service';

const getNotificationforUser = (data: any) => {
  return useQuery(
    ['getNotificationforUser'],
    () => notificationService.filterNotification(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const getNewNotificationByUserID = (id: number) => {
  return useQuery(
    ['getNewNotificationByUserID', id],
    () => notificationService.getNotificationCountByUserID(id),
    {
      select: (data) => data.data,
    }
  );
};

const updateNotificationStatus = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return notificationService.updateNotificationStatusByUser(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getNewNotificationByUserID']);
      },
    }
  );
};

export {
  getNotificationforUser,
  getNewNotificationByUserID,
  updateNotificationStatus,
};
