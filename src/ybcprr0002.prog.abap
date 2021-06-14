*----------------------------------------------------------------------*
* Descripción:  This program will calculate a hash by object and compare
*               it between the landscape
*----------------------------------------------------------------------*

REPORT  ybcprr0002.

INCLUDE ybcprr0002_top.
INCLUDE ybcprr0002_sel.
INCLUDE ybcprr0002_c01.
INCLUDE ybcprr0002_f01.
INCLUDE ybcprr0002_o01.
INCLUDE ybcprr0002_i01.

INITIALIZATION.

  PERFORM fill_out_of_leveling.

AT SELECTION-SCREEN.

  IF sy-ucomm EQ 'ONLI'. " Only search when execute
    PERFORM object_selection.
    PERFORM subobject_selection.
    PERFORM remove_not_custom_object.
    PERFORM remove_deleted_objects.
  ENDIF.

START-OF-SELECTION.

  PERFORM analyze.

  CALL SCREEN 100.
