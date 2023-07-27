
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

create table user_profiles (
    user_profile_id serial4 not null,
    user_id int4 not null,
    profile_image_url varchar(255) null,
    date_of_birth date null,
    gender varchar(10) null,
    address jsonb null,
    additional_info jsonb null,
    created_date timestamptz not null,
	updated_date timestamptz not null,
	created_by int8 null,
	updated_by int8 null,
	is_delete boolean not null default false,
    constraint pk_user_profile primary key (user_profile_id),
    constraint fk_user_profile_user_id foreign key (user_id) references users (user_id), 
    constraint uk_user_profile_user_id unique(user_id)
);

alter table users 
drop column address;

alter table users 
add column address text null;
