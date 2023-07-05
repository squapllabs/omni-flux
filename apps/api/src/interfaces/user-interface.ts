interface CreateUserBody {
  center_id: BigInteger;
  user_name: string;
  user_password: string;
  mobile_number: string;
  email_id: string;
  first_name: string;
  last_name: string;
  profile_img_url: string;
  gender: string;
  dob: Date;
  status: string;
  address: JSON;
  created_by: BigInteger;
  updated_by: BigInteger;
  role_id: BigInteger;
}

export { CreateUserBody };
