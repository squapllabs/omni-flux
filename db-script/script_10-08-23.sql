drop table site_expense ;

create table site_expense(
site_expense_id serial4 not null,
site_id int4,
project_id int4,
employee_name varchar(50),
employee_id varchar(20),
employee_phone varchar(20),
purpose varchar(50),
department varchar(50),
designation varchar(50),
start_date date,
end_date date,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int8,
updated_by int8,
constraint pk_site_expense primary key (site_expense_id),
constraint fk_site_expense_site_id foreign key (site_id) references site_contractor(site_contractor_id),
constraint fk_site_expense_project_id foreign key (project_id) references project(project_id)
)

create table site_expense_details(
site_expense_details_id serial4 not null,
site_expense_id int4 not null,
description varchar(250),
air_transport double precision,
fuel double precision,
labour_advance double precision,
phone_stationary double precision,
food_snacks double precision,
purchase_service double precision,
others double precision,
total double precision,
bill_details jsonb,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int8,
updated_by int8,
constraint pk_site_expense_details primary key (site_expense_details_id),
constraint fk_site_expense_details_site_expense_id foreign key (site_expense_id) references site_expense(site_expense_id)
)

alter table site_expense_details 
drop column is_delete;

alter table project 
drop column currency;