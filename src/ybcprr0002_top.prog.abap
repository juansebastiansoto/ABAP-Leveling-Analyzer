
TYPES: BEGIN OF ty_screen,
         obj_name     TYPE e071-obj_name,
         trkorr       TYPE e071-trkorr,
         smtp_address TYPE rcf_receiver_email,
       END OF ty_screen.

DATA: o_alv_0100 TYPE REF TO cl_gui_alv_grid,
      o_cc_0100  TYPE REF TO cl_gui_custom_container.

DATA: t_result TYPE ybctt0000.

DATA: w_screen TYPE ty_screen.
