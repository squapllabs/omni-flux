alter table inventory 
drop column item_id;

alter table vendor_quotes 
rename column quatation_details to quotation_details;

alter table indent_request 
rename column approvar_user_id to approver_user_id;

alter table indent_request 
rename column approvar_status to approver_status;

alter table indent_request 
rename column approvar_comments to approver_comments;

create table purchase_order(
purchase_order_id serial4 not null,
purchase_request_id int4,
vendor_id int4,
order_date date,
status varchar(50),
total_cost float8,
order_remark varchar(300),
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_purchase_order primary key (purchase_order_id),
constraint fk_purchase_order_purchase_request_id foreign key (purchase_request_id) references purchase_request(purchase_request_id),
constraint fk_purchase_order_vendor_id foreign key (vendor_id) references vendor(vendor_id)
)

create table purchase_order_item(
purchase_order_item_id serial4 not null,
purchase_order_id int4 not null,
item_id int4,
order_quantity int4,
unit_price float8,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_purchase_order_item primary key (purchase_order_item_id) ,
constraint fk_purchase_order_item_purchase_order_id foreign key (purchase_order_id) references purchase_order(purchase_order_id),
constraint fk_purchase_order_item_item_id foreign key (item_id) references item(item_id)
)