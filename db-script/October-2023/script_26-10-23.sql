create table purchase_request_quotation_details (
purchase_request_quotation_details_id serial4 not null,
purchase_request_id int4 not null,
item_id int4,
indent_request_details_id int4,
indent_requested_quantity int4,
purchase_requested_quantity int4,
unit_cost float8,
total_cost float8,
is_delete bool not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int8 null,
updated_by int8 null,
constraint pk_purchase_request_quotation_details primary key (purchase_request_quotation_details_id),
constraint fk_purchase_request_quotation_details_purchase_request_id foreign key (purchase_request_id) references purchase_request(purchase_request_id),
constraint fk_purchase_request_quotation_details_item_id foreign key (item_id) references item(item_id),
constraint fk_purchase_request_quotation_details_indent_request_details_id foreign key (indent_request_details_id) references indent_request_details(indent_request_details_id)
)


create table vendor_quotation_details (
vendor_quotation_details_id serial4 not null,
vendor_quotes_id int4,
item_id int4,
indent_request_details_id int4,
indent_requested_quantity int4,
purchase_requested_quantity int4,
unit_cost float8,
total_cost float8,
is_delete bool not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int8 null,
updated_by int8 null,
constraint pk_vendor_quotation_details primary key (vendor_quotation_details_id),
constraint fk_vendor_quotation_details_vendor_quotes_id foreign key (vendor_quotes_id) references vendor_quotes(vendor_quotes_id),
constraint fk_vendor_quotation_details_item_id foreign key (item_id) references item(item_id),
constraint fk_vendor_quotation_details_indent_request_details_id foreign key (indent_request_details_id) references indent_request_details(indent_request_details_id)
)

alter table purchase_request 
drop column purchase_request_details;

alter table vendor_quotes 
drop column quotation_details;