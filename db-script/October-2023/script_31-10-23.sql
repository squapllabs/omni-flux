alter table purchase_order 
add column purchase_order_type varchar(50);

alter table purchase_order 
add column indent_request_id int4;

alter table purchase_order 
add constraint fk_purchase_order_indent_request_id foreign key (indent_request_id) references indent_request(indent_request_id);

create table grn(
grn_id serial4 not null,
project_id int4,
purchase_order_id int4,
goods_received_by int4,
goods_received_date date,
invoice_id varchar(50),
bill_details jsonb,
grn_status varchar(20),
created_date timestamptz not null default now(),
created_by int8 null,
constraint pk_grn primary key (grn_id),
constraint fk_grn_project_id foreign key (project_id) references project(project_id),
constraint fk_grn_purchase_order_id foreign key (purchase_order_id) references purchase_order(purchase_order_id),
constraint fk_grn_goods_received_by foreign key (goods_received_by) references users(user_id)
)

create table grn_details(
grn_details_id serial4 not null,
grn_id int4,
item_id int4,
received_quantity int4,
accepted_quantity int4,
rejected_quantity int4,
notes text,
created_date timestamptz not null default now(),
created_by int8 null,
constraint pk_grn_details primary key (grn_details_id),
constraint fk_grn_details_grn_id foreign key (grn_id) references grn(grn_id),
constraint fk_grn_details_item_id foreign key (item_id) references item(item_id)
)
