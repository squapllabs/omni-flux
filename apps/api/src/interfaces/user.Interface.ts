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
  department: string;
}

interface updateUserBody {
  first_name: string;
  last_name: string;
  address: string;
  updated_by: bigint;
  user_id: number;
  department: string;
  role_id: bigint;
}

export { createUserBody, updateUserBody };
