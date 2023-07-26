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

interface userExistBody {
  user_id: number;
  first_name: string;
  last_name: string;
  user_password: string;
  contact_no: string;
  email_id: string;
  user_status: string;
  address: string;
  created_by: number;
  created_date: Date;
  updated_by: number;
  updated_date: Date;
  is_delete: boolean;
  is_initial_login: boolean;
  is_two_factor: boolean;
  otp_secret: number;
  otp_attempts: number;
  otp_expired_in: Date;
  is_otp_verified: boolean;
  department: string;
}
export { createUserBody, updateUserBody, userExistBody };
