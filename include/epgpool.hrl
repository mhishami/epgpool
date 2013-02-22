
-define (INFO(Name), error_logger:info_msg(Name)).
-define (INFO(Name, Parms), error_logger:info_msg(Name, Parms)).

-define (WARN(Name), error_logger:warning_msg(Name)).
-define (WARN(Name, Parms), error_logger:warning_msg(Name, Parms)).

-define (ERROR(Name), error_logger:error_msg(Name)).
-define (ERROR(Name, Parms), error_logger:error_msg(Name, Parms)).
