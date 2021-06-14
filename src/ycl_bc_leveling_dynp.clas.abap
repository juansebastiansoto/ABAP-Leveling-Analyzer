class YCL_BC_LEVELING_DYNP definition
  public
  inheriting from YCL_BC_LEVELING
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !OBJECT type CTS_OBJECT_KEY
    raising
      CX_T100_MSG
      CX_SY_CREATE_OBJECT_ERROR .

  methods GET_OBJECT_LIST
    redefinition .
protected section.

  methods DETERMINE_TEXT_OBJECT_CODE
    redefinition .
  methods EXISTENCE_CHECK
    redefinition .
  methods GET_OBJECT_CONTENT
    redefinition .
private section.

  methods CHECK_SELECTION_SCREEN
    returning
      value(RE_SKIP) type BOOLEAN .
ENDCLASS.



CLASS YCL_BC_LEVELING_DYNP IMPLEMENTATION.


METHOD check_selection_screen.

  DATA: tl_d020s       TYPE STANDARD TABLE OF d020s.

  DATA: vl_object_name TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_d020s> TYPE d020s.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_DYNP_40'
    EXPORTING
      object_name = vl_object_name
      versno      = '00000'
    TABLES
      d020s_tab   = tl_d020s
    EXCEPTIONS
      OTHERS      = 0.

  READ TABLE tl_d020s ASSIGNING <fsl_d020s> INDEX 1.

  IF  <fsl_d020s> IS ASSIGNED
  AND <fsl_d020s>-type CA ' WJS'. " Selection Screen
    re_skip = abap_true.
  ENDIF.

ENDMETHOD.


METHOD constructor.

  CALL METHOD super->constructor
    EXPORTING
      object = object.

  IF  object-object IN me->get_object_list( )
  AND ( me->existence_check( ) EQ abap_false OR me->check_selection_screen( ) EQ abap_true ).
    RAISE EXCEPTION TYPE cx_sy_create_object_error.
  ENDIF.

ENDMETHOD.


METHOD DETERMINE_TEXT_OBJECT_CODE.

  object_type = 'DYNP'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_DYNP'
    DESTINATION server
    EXPORTING
      limu_key  = me->object-obj_name
    EXCEPTIONS
      not_exist = 1
      OTHERS    = 2.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  exist = abap_true.

ENDMETHOD.


METHOD get_object_content.

  DATA: tl_d020s TYPE STANDARD TABLE OF d020s,
        tl_d020t TYPE STANDARD TABLE OF d020t,
        tl_d021s TYPE STANDARD TABLE OF d021se,
        tl_d022s TYPE STANDARD TABLE OF d022s.

  DATA: vl_object_name TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_d020s> TYPE d020s.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_DYNP_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      d020s_tab             = tl_d020s
      d020t_tab             = tl_d020t
      d021s_tab             = tl_d021s
      d022s_tab             = tl_d022s
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  LOOP AT tl_d020s ASSIGNING <fsl_d020s>.
    CLEAR: <fsl_d020s>-dgen, <fsl_d020s>-tgen.
  ENDLOOP.

  SORT tl_d020t.

  me->add_to_string( EXPORTING input = tl_d020s  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_d020t  CHANGING output = object_content ).
*  me->add_to_string( EXPORTING input = tl_d021s  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_d022s  CHANGING output = object_content ).

ENDMETHOD.


METHOD GET_OBJECT_LIST.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'DYNP'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
