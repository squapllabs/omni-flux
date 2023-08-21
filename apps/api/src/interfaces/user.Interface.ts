interface createUserBody {
  user_password: string;
  contact_no: string;
  email_id: string;
  first_name: string;
  last_name: string;
  user_status: string;
  address: JSON;
  created_by: bigint;
  updated_by: bigint;
  role_id: bigint;
  is_delete: boolean;
  department: string;
  profile_image_url: string;
  date_of_birth: Date;
  gender: string;
  additional_info: JSON;
  is_two_factor: boolean;
  parent_user_id: number;
}

interface updateUserBody {
  first_name: string;
  last_name: string;
  address: JSON;
  updated_by: bigint;
  user_id: number;
  department: string;
  role_id: bigint;
  profile_image_url: string;
  date_of_birth: Date;
  gender: string;
  additional_info: JSON;
  is_two_factor: boolean;
  parent_user_id: number;
}

export { createUserBody, updateUserBody };
