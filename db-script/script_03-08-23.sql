alter table lead_enquiry_product_item 
drop column lead_enquiry_id;

alter table lead_enquiry_product_item 
add column lead_enquiry_product_id int4 ;

alter table lead_enquiry_product_item 
add constraint fk_lead_enquiry_product_item_lead_enquiry_product_id foreign key (lead_enquiry_product_id) references lead_enquiry_product(lead_product_id);    
