alter table project 
add column project_documents jsonb;

update project 
set project_documents='[{"index": 0,"path": "https://zpaisa-purchase-sale-docs.s3.ap-south-1.amazonaws.com/file-1691582320217-181824419-file-1691557004931-678480558-sample.pdf"}]'

alter table project 
drop column document_url;