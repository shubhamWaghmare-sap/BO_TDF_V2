"! <p class="shorttext synchronized" lang="en">Generic Test Double for Abap Behavior Test Environment</p>
"! <p> This is a generic test double which can be used for abap behavior test environment. It provides some basic handling for read and modify.
"! If this enough for the test case, then an instance of this test double can be passed on to the abap behavior test environment.
"! If more custom logic is required, override the methods an extend those methods with the custom logic required. Then an instance of that
"! test double can be passed on to the environment </p>
class zcl_abap_behv_generic_test_dbl definition public create public for testing.
  public section.
    interfaces if_abap_behavior_testdouble.
    methods:
      constructor
        importing root_name type abp_root_entity_name.
    methods: config_error_response_4_modify
      importing reported type data
                failed   type data.
    methods: config_error_response_4_read
      importing reported type data
                failed   type data.


  protected section.
  private section.
endclass.

class zcl_abap_behv_generic_test_dbl implementation.

  method constructor.
    if_abap_behavior_testdouble~root_name = to_upper( root_name ).

    " retrieve bdef load info
    " NOT GETTING EXECUTED
    " ALL THE IMPORTED PARAMETERS ARE EMPTY
*    cl_abap_behv_load=>get_load(
*                     exporting
*                       entity   = root_name
*                       all      = abap_true
*                     importing
*                       head         = bdef_load-head
*                       entities     = bdef_load-entities
*                       features     = bdef_load-features
*                       actions      = bdef_load-actions
*                       relations    = bdef_load-relations
*                       associations = bdef_load-associations ).

  endmethod.

  method if_abap_behavior_testdouble~read.
    " read from double transactional buffer
    zcl_abap_behv_test_trans_bufr2=>get_instance( )->read(
      exporting
        root_name  = if_abap_behavior_testdouble~root_name
        retrievals = retrievals
      changing
        failed     = failed
        reported   = reported
    ).
  endmethod.

  method if_abap_behavior_testdouble~modify.
    " modify double transactional buffer
    zcl_abap_behv_test_trans_bufr2=>get_instance( )->modify(
      exporting
        root_name = if_abap_behavior_testdouble~root_name
        changes   = changes
      changing
        failed    = failed
        mapped    = mapped
        reported  = reported
    ).
  endmethod.

  method config_error_response_4_modify.

    zcl_abap_behv_test_trans_bufr2=>get_instance( )->config_error_response_4_modify(
     exporting
     root_name = if_abap_behavior_testdouble~root_name
     reported = reported
     failed = failed ).

  endmethod.

  method config_error_response_4_read.

    zcl_abap_behv_test_trans_bufr2=>get_instance( )->config_error_response_4_read(
      exporting
        root_name = if_abap_behavior_testdouble~root_name
        reported = reported
        failed = failed ).

  endmethod.

endclass.
