class YCL_BC_LEVELING_DTED definition
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



CLASS YCL_BC_LEVELING_DTED IMPLEMENTATION.


METHOD constructor.

  CALL METHOD super->constructor
    EXPORTING
      object = object.

  IF  object-object IN me->get_object_list( )
  AND me->existence_check( ) EQ abap_false.
    RAISE EXCEPTION TYPE cx_sy_create_object_error.
  ENDIF.

ENDMETHOD.


METHOD DETERMINE_TEXT_OBJECT_CODE.

  object_type = 'DTEL'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_DTEL'
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

  DATA: tl_dd04v   TYPE STANDARD TABLE OF dd04v,
        tl_dd04vd  TYPE STANDARD TABLE OF dd04vd.

  DATA: wl_dd04v   TYPE dd04v,
        wl_dd04vd  TYPE dd04vd.

  DATA: vl_object_name TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_dd04v>  TYPE dd04v,
                 <fsl_dd04vd> TYPE dd04vd.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_DTED_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      dd04v_tab             = tl_dd04v
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  LOOP AT tl_dd04v ASSIGNING <fsl_dd04v>.

    APPEND INITIAL LINE TO tl_dd04vd ASSIGNING <fsl_dd04vd>.

    MOVE-CORRESPONDING <fsl_dd04v> to <fsl_dd04vd>.

*   If is defined by domain, clear data type properties
    IF <fsl_dd04vd>-domname IS NOT INITIAL.
      CLEAR: <fsl_dd04vd>-datatype, <fsl_dd04vd>-leng, <fsl_dd04vd>-decimals.
    ENDIF.

  ENDLOOP.

  me->add_to_string( EXPORTING input = tl_dd04vd  CHANGING output = object_content ).

ENDMETHOD.


METHOD GET_OBJECT_LIST.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'DTEL'.
  APPEND wl_list TO list.

  wl_list-low    = 'DTED'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
