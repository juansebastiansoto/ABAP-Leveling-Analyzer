class YCL_BC_LEVELING_SHLP definition
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



CLASS YCL_BC_LEVELING_SHLP IMPLEMENTATION.


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

  object_type = 'SHLP'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_SHLP'
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

  DATA: tl_dd30v  TYPE STANDARD TABLE OF dd30v,
        tl_dd30vd TYPE STANDARD TABLE OF dd30vd,
        tl_dd31v  TYPE STANDARD TABLE OF dd31v,
        tl_dd32v  TYPE STANDARD TABLE OF dd32v,
        tl_dd32vd TYPE STANDARD TABLE OF dd32vd,
        tl_dd33v  TYPE STANDARD TABLE OF dd33v.

  DATA: vl_object_name  TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_dd30v>  TYPE dd30v,
                 <fsl_dd30vd> TYPE dd30vd,
                 <fsl_dd32v>  TYPE dd32v,
                 <fsl_dd32vd> TYPE dd32vd.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_SHLD_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      dd30v_tab             = tl_dd30v
      dd31v_tab             = tl_dd31v
      dd32v_tab             = tl_dd32v
      dd33v_tab             = tl_dd33v
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  LOOP AT tl_dd30v ASSIGNING <fsl_dd30v>.

    APPEND INITIAL LINE TO tl_dd30vd ASSIGNING <fsl_dd30vd>.
    MOVE-CORRESPONDING <fsl_dd30v> TO <fsl_dd30vd>.

  ENDLOOP.

  LOOP AT tl_dd32v ASSIGNING <fsl_dd32v>.

    APPEND INITIAL LINE TO tl_dd32vd ASSIGNING <fsl_dd32vd>.
    MOVE-CORRESPONDING <fsl_dd32v> TO <fsl_dd32vd>.

  ENDLOOP.

  me->add_to_string( EXPORTING input = tl_dd30vd CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd31v  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd32vd CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd33v  CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'SHLP'.
  APPEND wl_list TO list.

  wl_list-low    = 'SHLX'.
  APPEND wl_list TO list.

  wl_list-low    = 'SHLD'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
