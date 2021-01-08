"! <p class="shorttext synchronized" lang="en">Generic Test Double for Abap Behavior Test Environment</p>
"! <p> This is a generic test double which can be used for abap behavior test environment. It provides some basic handling for read and modify.
"! If this enough for the test case, then an instance of this test double can be passed on to the abap behavior test environment.
"! If more custom logic is required, override the methods an extend those methods with the custom logic required. Then an instance of that
"! test double can be passed on to the environment </p>
class zcl_abap_behv_generic_tst_dbl5 definition public create public for testing.
  public section.
    interfaces if_abap_behavior_testdouble.
    methods:

      "! <p class="shorttext synchronized" lang="EN">Constructor of Generic Test Double for a BO</p>
      "! Response structures like mapped/reported/failed/result for an EML MODIFY statement can be configured using this API.
      "! After setting the test double in the environment, the EML MODIFY statements will return responses configured with this API.
      "! @parameter root_name | Set the root entity name of the BO to Mock
      "! @parameter iv_handle_draft  | Set to true if the BO is draft enabled. Default value: False
      constructor
        importing root_name         type abp_root_entity_name
                  !iv_handle_draft  type char1 default abap_false,

      "! <p class="shorttext synchronized" lang="EN">Configures Response Structures of MODIFY EML statements</p>
      "! Response structures like mapped/reported/failed/result for an EML MODIFY statement can be configured using this API.
      "! After setting the test double in the environment, the EML MODIFY statements will return responses configured with this API.
      config_response_4_modify
        importing mapped    type data optional
                  reported  type data optional
                  failed    type data optional
                  operation type abp_behv_op_modify,

      "! <p class="shorttext synchronized" lang="EN">Configures Response Structures of READ EML statements</p>
      "! Response structures like mapped/reported/failed/result for an EML READ statement can be configured using this API.
      "! After setting the test double in the environment, the EML READ statements will return responses configured with this API.
      config_response_4_read
        importing result    type standard table optional
                  reported  type data optional
                  failed    type data optional
                  operation type abp_behv_op_read,

      "! <p class="shorttext synchronized" lang="EN">Clears Double Transactional Buffer for the BO double.</p>
      "! Deletes all created entity instances from the Buffer.
      clear_buffer.

  protected section.
  private section.
endclass.

class zcl_abap_behv_generic_tst_dbl5 implementation.

  method constructor.
    if_abap_behavior_testdouble~root_name = to_upper( root_name ).
    if_abap_behavior_testdouble~handle_draft = iv_handle_draft.
  endmethod.

  method if_abap_behavior_testdouble~read.
    " read from double transactional buffer
    zcl_abap_behv_test_trans_bufr5=>get_instance( )->read(
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
    zcl_abap_behv_test_trans_bufr5=>get_instance( )->modify(
      exporting
        root_name = if_abap_behavior_testdouble~root_name
        changes   = changes
      changing
        failed    = failed
        mapped    = mapped
        reported  = reported
    ).
  endmethod.

  method config_response_4_modify.

    zcl_abap_behv_test_trans_bufr5=>get_instance( )->config_response_4_modify(
     exporting
     root_name = if_abap_behavior_testdouble~root_name
     operation = operation
     mapped    = mapped
     reported  = reported
     failed    = failed ).

  endmethod.

  method config_response_4_read.

    zcl_abap_behv_test_trans_bufr5=>get_instance( )->config_response_4_read(
      exporting
        root_name = if_abap_behavior_testdouble~root_name
        operation = operation
        result   = result
        reported = reported
        failed = failed ).

  endmethod.

  method clear_buffer.
    zcl_abap_behv_test_trans_bufr5=>get_instance( )->clear_buffer(  ).
  endmethod.

endclass.
