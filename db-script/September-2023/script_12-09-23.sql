alter table store 
ALTER COLUMN created_by TYPE integer USING created_by::integer;

alter table store 
ALTER COLUMN updated_by TYPE integer USING updated_by::integer;

create table indent_request(
indent_request_id serial4 not null,
requester_user_id int4 not null,
requested_date date not null,
request_status varchar(50),
priority varchar(50),
description text,
expected_delivery_date date,
total_cost double precision,
approvar_user_id int4 ,
approvar_status varchar(50),
approved_date date,
rejected_date date,
approvar_comments text,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_indent_request primary key (indent_request_id),
constraint fk_indent_request_requester_user_id foreign key (requester_user_id) references users(user_id),
constraint fk_indent_request_approvar_user_id foreign key (approvar_user_id) references users(user_id)
)

create table indent_request_details(
indent_request_details_id serial4 not null,
indent_request_id int4 not null,
bom_detail_id int4 not null,
quantity int4,
total double precision,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_indent_request_details primary key (indent_request_details_id),
constraint fk_indent_request_details_indent_request_id foreign key (indent_request_id) references indent_request(indent_request_id),
constraint fk_indent_request_details_bom_detail_id foreign key (bom_detail_id) references bom_detail(bom_detail_id)
)

create table inventory (
inventory_id serial4 not null,
item_id int4 not null,
item_name varchar(50),
item_category varchar(50),
rate double precision,
available_quantity int4 not null,
store_id int4 not null,
project_id int4,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_inventory primary key (inventory_id),
constraint fk_inventory_store_id foreign key (store_id) references store(store_id),
constraint fk_inventory_item_id foreign key (item_id) references item(item_id),
constraint fk_inventory_project_id foreign key (project_id) references project(project_id)
)

alter table indent_request 
add column project_id int4;

alter table indent_request 
add constraint fk_indent_request_project_id foreign key (project_id) references project(project_id);