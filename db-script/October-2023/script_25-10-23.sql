alter table indent_request 
add column request_type varchar(20);

alter table purchase_request  
add column purchase_request_code varchar(50);

create sequence purchase_request_code_sequence
start 1
increment 1;

select concat('PUR',DATE_PART('year', CURRENT_DATE),'00',nextval('purchase_request_code_sequence')::text) as purchase_request_code_sequence;
