class YCL_BC_LEVELING_METH definition
  public
  inheriting from YCL_BC_LEVELING_CLAS
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



CLASS YCL_BC_LEVELING_METH IMPLEMENTATION.


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

  object_type = 'METH'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_METH'
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

  DATA: tl_source       TYPE STANDARD TABLE OF abaptxt255.

  DATA: vl_object_name  TYPE vrsd-objname.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_METH_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      repos_tab             = tl_source
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  me->add_to_string( EXPORTING input = tl_source  CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'METH'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
