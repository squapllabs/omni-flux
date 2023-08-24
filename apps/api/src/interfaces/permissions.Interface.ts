interface permissionsBody {
  permission_id: number;
  is_create: boolean;
  is_update: boolean;
  is_read: boolean;
  is_visible: boolean;
  capability_id: number;
  role_id: number;
  created_by: number;
  updated_by: number;
}

export { permissionsBody };
