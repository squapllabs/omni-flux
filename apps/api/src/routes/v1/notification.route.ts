import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  createNotification,
  getAllNotifications,
  getByNotificationId,
  getUnreadCountByToUserId,
  searchNotification,
  updateIsReadByToUserId,
} from '../../controller/notification.controller';

const router = express.Router();

router.post('/', authMiddleware, createNotification);

router.get('/get-all', authMiddleware, getAllNotifications);

router.get('/get/:notification_id', authMiddleware, getByNotificationId);

router.post('/search', authMiddleware, searchNotification);

router.get(
  '/get-unread-count-by-to-user-id/:notification_to_user_id',
  authMiddleware,
  getUnreadCountByToUserId
);

router.put(
  '/mark-all-as-read-by-to-user-id',
  authMiddleware,
  updateIsReadByToUserId
);

export default router;
