interface CreateUserBody {
  user_password: string;
  contact_no: string;
  email_id: string;
  first_name: string;
  last_name: string;
  user_status: string;
  address: string;
  created_by: BigInteger;
  updated_by: BigInteger;
  role_id: BigInteger;
  is_delete: boolean;
}

export { CreateUserBody };
