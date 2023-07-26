
alter table hsn_code 
add column is_delete boolean not null default false;

alter table users 
add column is_otp_verified boolean default false;

alter table users 
add column otp_secret varchar(20);

alter table users 
add column otp_attempts int4;

alter table users 
add column otp_last_sent timestamptz;

alter table users 
add column is_otp_verified boolean default false;

alter table users
rename column otp_last_sent to otp_expired_in ;

alter table users 
alter column otp_secret type int4
using otp_secret::int4;
