alter table grn 
add column invoice_amount float8 ;

-- Purchase Order Invoice Table Script

create table purchase_order_invoice(
purchase_order_invoice_id serial4 not null,
purchase_order_id int4 not null,
grn_id int4 not null,
invoice_number varchar(30),
invoice_document jsonb ,
requested_by int4,
invoice_date date,
due_date date,
status varchar(20),
additional_info jsonb,
total_amount float8,
paid_by int4,
paid_date int4,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int8 null,
updated_by int8 null,
constraint pk_purchase_order_invoice_id primary key (purchase_order_invoice_id),
constraint fk_purchase_order_invoice_purchase_order_id foreign key (purchase_order_id) references purchase_order(purchase_order_id),
constraint fk_purchase_order_invoice_grn_id foreign key (grn_id) references grn(grn_id),
constraint fk_purchase_order_invoice_requested_by foreign key (requested_by) references users(user_id),
constraint fk_purchase_order_invoice_paid_by foreign key (paid_by) references users(user_id)
)