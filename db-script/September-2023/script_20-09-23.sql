alter table purchase_order 
add column order_id varchar(100);

select concat('PO',DATE_PART('year', CURRENT_DATE),'00',nextval('po_sequence')::text) as order_id_sequence;

create table project_inventory(
project_inventory_id serial4 not null,
project_id int4 not null,
item_id int4,
rate float8,
available_quantity int4,
total_cost float8,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_project_inventory primary key (project_inventory_id),
constraint fk_project_inventory_project_id foreign key (project_id) references project(project_id),
constraint fk_project_inventory_item_id foreign key (item_id) references item(item_id)
)

create table stock_outward(
stock_outward_id serial4 not null,
outward_id varchar(100) not null,
project_id int4,
site_id int4,
site_engineer_id int4,
item_count int4,
stock_outward_date date,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_stock_outward primary key (stock_outward_id),
constraint fk_stock_outward_project_id foreign key (project_id) references project(project_id),
constraint fk_stock_outward_site_id foreign key (site_id) references site_contractor(site_contractor_id),
constraint fk_stock_outward_site_engineer_id foreign key (site_engineer_id) references users(user_id)
)

create table stock_outward_details(
stock_outward_details_id serial4 not null,
stock_outward_id int4,
item_id int4,
outward_quantity int4,
uom_id int4,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_stock_outward_details primary key (stock_outward_details_id),
constraint fk_stock_outward_details_stock_outward_id foreign key (stock_outward_id) references stock_outward(stock_outward_id),
constraint fk_stock_outward_details_item_id foreign key (item_id) references item(item_id),
constraint fk_stock_outward_details_uom_id foreign key (uom_id) references uom(uom_id)
)

create sequence sto_sequence
    start 1
    increment 1;

select concat('STO',DATE_PART('year', CURRENT_DATE),'00',nextval('sto_sequence')::text) as stock_outward_sequence;

