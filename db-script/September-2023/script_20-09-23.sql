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