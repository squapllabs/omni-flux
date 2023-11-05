create table notifications(
notification_id serial4 NOT NULL, --1
notification_from_user_id int4, --1
notification_to_user_id int4, --60
notification_type_id int4, --indent_request_id 
notification_type varchar(50),--IndentRequest
notification_description text, -- when user creates an indent request - the notification should be sent to it's approver (i.e., Project Manager )
is_read boolean not null default false, -- false 
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int8, --1
updated_by int8,
constraint pk_notifications primary key (notification_id),
constraint fk_notifications_notification_from_user_id foreign key (notification_from_user_id) references users(user_id),
constraint fk_notifications_notification_to_user_id foreign key (notification_to_user_id) references users(user_id)
)