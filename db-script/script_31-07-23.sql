alter table client
add column is_delete boolean not null default false;

drop table lead_product ;

drop table lead_tender ;

drop table lead_enquiry ;
