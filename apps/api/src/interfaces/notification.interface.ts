interface notificationBody {
  notification_id: number;
  notification_from_user_id: number;
  notification_to_user_id: number;
  notification_type_id: number;
  notification_type: string;
  notification_description: string;
  is_read: boolean;
  created_by: number;
  updated_by: number;
}

export { notificationBody };
