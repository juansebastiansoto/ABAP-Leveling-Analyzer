class YCL_BC_LEVELING_TABL definition
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



CLASS YCL_BC_LEVELING_TABL IMPLEMENTATION.


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

  DATA: vl_tabclass TYPE dd02l-tabclass,
        vl_tabname  TYPE dd02l-tabname.

  SELECT SINGLE tabclass
  FROM dd02l
  INTO vl_tabclass
  WHERE tabname  EQ vl_tabname
    AND as4local EQ 'A'
    AND as4vers  EQ ''.

  CASE vl_tabclass.
    WHEN 'INTTAB' OR 'APPEND'. " Structure
      object_type = 'STRU'.

    WHEN 'TRANSP'. " Table
      object_type = 'TABL'.

  ENDCASE.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_TABL'
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

  DATA: tl_dd02v  TYPE STANDARD TABLE OF dd02v,
        tl_dd02vd TYPE STANDARD TABLE OF dd02vd,
        tl_dd02tv TYPE STANDARD TABLE OF dd02tv,
        tl_dd03v  TYPE STANDARD TABLE OF dd03v,
        tl_dd05v  TYPE STANDARD TABLE OF dd05v,
        tl_dd08v  TYPE STANDARD TABLE OF dd08v.

  DATA: vl_object_name TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_dd02v>  TYPE dd02v,
                 <fsl_dd02vd> TYPE dd02vd,
                 <fsl_dd03v>  TYPE dd03v.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_TABD_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      dd02tv_tab            = tl_dd02tv
      dd02v_tab             = tl_dd02v
      dd03v_tab             = tl_dd03v
      dd05v_tab             = tl_dd05v
      dd08v_tab             = tl_dd08v
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  LOOP AT tl_dd02v ASSIGNING <fsl_dd02v>.

    APPEND INITIAL LINE TO tl_dd02vd ASSIGNING <fsl_dd02vd>.

    MOVE-CORRESPONDING <fsl_dd02v> TO <fsl_dd02vd>.

  ENDLOOP.

  LOOP AT tl_dd03v ASSIGNING <fsl_dd03v>.
    CLEAR: <fsl_dd03v>-domname, <fsl_dd03v>-shlporigin, <fsl_dd03v>-ddlanguage, <fsl_dd03v>-ddtext.

    IF <fsl_dd03v>-rollname IS NOT INITIAL. " In fields with data element, the data element properties is empty.
      CLEAR: <fsl_dd03v>-intlen, <fsl_dd03v>-datatype, <fsl_dd03v>-leng, <fsl_dd03v>-decimals.
    ENDIF.

  ENDLOOP.

  me->add_to_string( EXPORTING input = tl_dd02tv CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd02vd  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd03v  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd05v  CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_dd08v  CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'TABL'.
  APPEND wl_list TO list.

  wl_list-low    = 'TABD'.
  APPEND wl_list TO list.

  wl_list-low    = 'TABT'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
