class YCL_BC_LEVELING_CUAD definition
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



CLASS YCL_BC_LEVELING_CUAD IMPLEMENTATION.


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

    object_type = 'CUAD'.

  ENDMETHOD.


METHOD existence_check.

    CALL FUNCTION 'RPY_EXISTENCE_CHECK_CUAD'
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

  DATA: tl_vact_40   TYPE STANDARD TABLE OF rsmpe_act,
        tl_vadm_40   TYPE STANDARD TABLE OF rsmpe_adm,
        tl_vbut_40   TYPE STANDARD TABLE OF rsmpe_but,
        tl_vdoc_40   TYPE STANDARD TABLE OF rsmpe_atr,
        tl_vfun_40   TYPE STANDARD TABLE OF rsmpe_fun,
        tl_vmen_40   TYPE STANDARD TABLE OF rsmpe_men,
        tl_vmtx_40   TYPE STANDARD TABLE OF rsmpe_mnl,
        tl_vpfk_40   TYPE STANDARD TABLE OF rsmpe_pfk,
        tl_vset_40   TYPE STANDARD TABLE OF rsmpe_staf,
        tl_vsta_40   TYPE STANDARD TABLE OF rsmpe_sta,
        tl_vtexts_40 TYPE STANDARD TABLE OF rsmptexts,
        tl_vtit_40   TYPE STANDARD TABLE OF rsmpe_tit.

  DATA: vl_object_name TYPE vrsd-objname.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_CUAD_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      vact_40_tab           = tl_vact_40
      vadm_40_tab           = tl_vadm_40
      vbut_40_tab           = tl_vbut_40
      vdoc_40_tab           = tl_vdoc_40
      vfun_40_tab           = tl_vfun_40
      vmen_40_tab           = tl_vmen_40
      vmtx_40_tab           = tl_vmtx_40
      vpfk_40_tab           = tl_vpfk_40
      vset_40_tab           = tl_vset_40
      vsta_40_tab           = tl_vsta_40
      vtexts_40_tab         = tl_vtexts_40
      vtit_40_tab           = tl_vtit_40
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  SORT tl_vdoc_40.
  SORT tl_vtexts_40.

  me->add_to_string( EXPORTING input = tl_vact_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vadm_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vbut_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vdoc_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vfun_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vmen_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vmtx_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vpfk_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vset_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vsta_40   CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vtexts_40 CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_vtit_40   CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

    DATA: wl_list TYPE LINE OF devtyrange.

    wl_list-sign   = 'I'.
    wl_list-option = 'EQ'.
    wl_list-low    = 'CUAD'.
    APPEND wl_list TO list.

  ENDMETHOD.
ENDCLASS.
