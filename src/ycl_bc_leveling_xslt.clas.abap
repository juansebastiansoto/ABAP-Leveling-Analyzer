class YCL_BC_LEVELING_XSLT definition
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



CLASS YCL_BC_LEVELING_XSLT IMPLEMENTATION.


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

  object_type = 'XSLT'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_XSLT'
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

  DATA: tl_xsatt TYPE STANDARD TABLE OF o2xsltattr,
        tl_xssrc TYPE STANDARD TABLE OF o2pageline,
        tl_xstxt TYPE STANDARD TABLE OF o2xslttext.

  DATA: vl_object_name TYPE vrsd-objname.

  FIELD-SYMBOLS: <fsl_xsatt> TYPE o2xsltattr.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_XSLT_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      xsatt_tab             = tl_xsatt
      xssrc_tab             = tl_xssrc
      xstxt_tab             = tl_xstxt
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  LOOP AT tl_xsatt ASSIGNING <fsl_xsatt>.
    CLEAR: <fsl_xsatt>-createdon, <fsl_xsatt>-changedby, <fsl_xsatt>-changedon.
  ENDLOOP.

  me->add_to_string( EXPORTING input = tl_xsatt CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_xssrc CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_xstxt CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'XSLT'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
