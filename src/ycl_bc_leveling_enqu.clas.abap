class YCL_BC_LEVELING_ENQU definition
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
ENDCLASS.



CLASS YCL_BC_LEVELING_ENQU IMPLEMENTATION.


METHOD constructor.

  CALL METHOD super->constructor
    EXPORTING
      object = object.

  IF  object-object IN me->get_object_list( )
  AND me->existence_check( ) EQ abap_false.
    RAISE EXCEPTION TYPE cx_sy_create_object_error.
  ENDIF.

ENDMETHOD.


METHOD determine_text_object_code.

  object_type = 'ENQU'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_ENQU'
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

  DATA: tl_dd25tv TYPE STANDARD TABLE OF dd25tv,
        tl_dd25v  TYPE STANDARD TABLE OF dd25v,
        tl_dd26v  TYPE STANDARD TABLE OF dd26v,
        tl_dd27v  TYPE STANDARD TABLE OF dd27v.

  DATA: vl_object_name TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_dd25v> TYPE dd25v,
                 <fsl_dd27v> TYPE dd27v.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_ENQD_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      dd25tv_tab            = tl_dd25tv
      dd25v_tab             = tl_dd25v
      dd26v_tab             = tl_dd26v
      dd27v_tab             = tl_dd27v
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  LOOP AT tl_dd25v ASSIGNING <fsl_dd25v>.
    CLEAR: <fsl_dd25v>-as4user, <fsl_dd25v>-as4date, <fsl_dd25v>-as4time.
  ENDLOOP.

  LOOP AT tl_dd27v ASSIGNING <fsl_dd27v>.
    CLEAR: <fsl_dd27v>-rollname, <fsl_dd27v>-ddlanguage.
  ENDLOOP.

  me->add_to_string( EXPORTING input = tl_dd25tv CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd25v  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd26v  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd27v  CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'ENQU'.
  APPEND wl_list TO list.

  wl_list-low    = 'ENQD'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
