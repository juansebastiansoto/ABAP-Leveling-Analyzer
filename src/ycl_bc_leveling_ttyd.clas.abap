class YCL_BC_LEVELING_TTYD definition
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



CLASS YCL_BC_LEVELING_TTYD IMPLEMENTATION.


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

  object_type = 'TTYP'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_TTYP'
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

  DATA: tl_dd40v     TYPE STANDARD TABLE OF dd40v,
        tl_dd40vd    TYPE STANDARD TABLE OF dd40vd,
        tl_dd42v     TYPE STANDARD TABLE OF dd42v,
        tl_dd42vd    TYPE STANDARD TABLE OF dd42vd.

  DATA: vl_object_name TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_dd40v>  TYPE dd40v,
                 <fsl_dd40vd> TYPE dd40vd,
                 <fsl_dd42v>  TYPE dd42v,
                 <fsl_dd42vd> TYPE dd42vd.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_TTYD_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      dd40v_tab             = tl_dd40v
      dd42v_tab             = tl_dd42v
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  LOOP AT tl_dd40v ASSIGNING <fsl_dd40v>.

    APPEND INITIAL LINE TO tl_dd40vd ASSIGNING <fsl_dd40vd>.

    MOVE-CORRESPONDING <fsl_dd40v> TO <fsl_dd40vd>.

  ENDLOOP.

  LOOP AT tl_dd42v ASSIGNING <fsl_dd42v>.

    APPEND INITIAL LINE TO tl_dd42vd ASSIGNING <fsl_dd42vd>.

    MOVE-CORRESPONDING <fsl_dd42v> TO <fsl_dd42vd>.

  ENDLOOP.

  me->add_to_string( EXPORTING input = tl_dd40vd CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd42vd CHANGING output = object_content ).

ENDMETHOD.


METHOD GET_OBJECT_LIST.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'TTYD'.
  APPEND wl_list TO list.

  wl_list-low    = 'TTYP'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
