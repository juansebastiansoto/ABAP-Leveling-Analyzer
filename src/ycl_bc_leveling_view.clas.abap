class YCL_BC_LEVELING_VIEW definition
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



CLASS YCL_BC_LEVELING_VIEW IMPLEMENTATION.


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

  object_type = 'VIEW'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_VIEW'
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

  DATA: tl_dd25tv      TYPE STANDARD TABLE OF dd25tv,
        tl_dd25v       TYPE STANDARD TABLE OF dd25v,
        tl_dd25vd      TYPE STANDARD TABLE OF dd25vd,
        tl_dd26v       TYPE STANDARD TABLE OF dd26v,
        tl_dd26vd      TYPE STANDARD TABLE OF dd26vd,
        tl_dd27v       TYPE STANDARD TABLE OF dd27v,
        tl_dd27vd      TYPE STANDARD TABLE OF dd27vd,
        tl_dd28v       TYPE STANDARD TABLE OF dd28v,
        tl_dd28vd      TYPE STANDARD TABLE OF dd28vd.

  DATA: vl_object_name TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_dd25v>  TYPE dd25v,
                 <fsl_dd26vd> TYPE dd26vd,
                 <fsl_dd26v>  TYPE dd26v,
                 <fsl_dd25vd> TYPE dd25vd,
                 <fsl_dd27v>  TYPE dd27v,
                 <fsl_dd27vd> TYPE dd27vd,
                 <fsl_dd28v>  TYPE dd28v,
                 <fsl_dd28vd> TYPE dd28vd.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_VIED_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      dd25v_tab             = tl_dd25v
      dd26v_tab             = tl_dd26v
      dd27v_tab             = tl_dd27v
      dd28v_tab             = tl_dd28v
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  LOOP AT tl_dd25v ASSIGNING <fsl_dd25v>.

    APPEND INITIAL LINE TO tl_dd25vd ASSIGNING <fsl_dd25vd>.

    MOVE-CORRESPONDING <fsl_dd25v> TO <fsl_dd25vd>.

  ENDLOOP.

  LOOP AT tl_dd26v ASSIGNING <fsl_dd26v>.

    APPEND INITIAL LINE TO tl_dd26vd ASSIGNING <fsl_dd26vd>.

    MOVE-CORRESPONDING <fsl_dd26v> TO <fsl_dd26vd>.

  ENDLOOP.

  LOOP AT tl_dd27v ASSIGNING <fsl_dd27v>.

    APPEND INITIAL LINE TO tl_dd27vd ASSIGNING <fsl_dd27vd>.

    MOVE-CORRESPONDING <fsl_dd27v> TO <fsl_dd27vd>.

  ENDLOOP.

  LOOP AT tl_dd28v ASSIGNING <fsl_dd28v>.

    APPEND INITIAL LINE TO tl_dd28vd ASSIGNING <fsl_dd28vd>.

    MOVE-CORRESPONDING <fsl_dd28v> TO <fsl_dd28vd>.

  ENDLOOP.

  me->add_to_string( EXPORTING input = tl_dd25vd  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd26vd  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd27vd  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd28vd  CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'VIEW'.
  APPEND wl_list TO list.

  wl_list-low    = 'VIED'.
  APPEND wl_list TO list.

  wl_list-low    = 'VIET'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
