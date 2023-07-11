interface createUserBody {
  user_password: string;
  contact_no: string;
  email_id: string;
  first_name: string;
  last_name: string;
  user_status: string;
  address: string;
  created_by: bigint;
  updated_by: bigint;
  role_id: bigint;
  is_delete: boolean;
}

interface updateUserBody {
  user_password: string;
  contact_no: string;
  email_id: string;
  first_name: string;
  last_name: string;
  user_status: string;
  address: string;
  created_by: bigint;
  updated_by: bigint;
  role_id: bigint;
  is_delete: boolean;
  user_id: bigint;
}

export { createUserBody, updateUserBody };
