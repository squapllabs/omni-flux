interface storeBody {
  store_id: number;
  store_name: string;
  store_manager_id: number;
  address: JSON;
  contact_email: string;
  contact_phone: string;
  project_id: number;
  site_id: number;
  created_by: number;
  updated_by: number;
}

export { storeBody };
