"! <p class="shorttext synchronized" lang="en">ABAP Behavior Test Transactional Buffer Double</p>
"!
"! <p> Interface to access the double transactional buffer used by generic test double for abap behavior test environment</p>
interface zif_abap_behv_test_trans_bufr6
  public .

    types:
      begin of ty_modify_response_config,
        operation type abp_behv_op_modify,
        mapped    type ref to data,
        reported  type ref to data,
        failed    type ref to data,
      end of   ty_modify_response_config.

    types:
      begin of ty_cid_to_inst_map,
        cid   type abp_behv_cid,
        inst  type ref to data ,
      end of   ty_cid_to_inst_map.

    types:
      begin of ty_read_response_config,
        operation type if_abap_behv=>t_char01,
        result    type ref to data,
        reported  type ref to data,
        failed    type ref to data,
      end of   ty_read_response_config.

    types: begin of ty_double_transactional_buffer,
             entity_root            type abp_root_entity_name,
             entity_name            type abp_entity_name,
             entity_alias           type abp_entity_name,
             entity_instances_w_cid type standard table of ty_cid_to_inst_map with key cid,
             entity_instances       type ref to data,
             modify_response_config type standard table of ty_modify_response_config with key operation,
             read_response_config   type standard table of ty_read_response_config with key operation,
           end of ty_double_transactional_buffer.

    types: tt_double_transactional_buffer type standard table of ty_double_transactional_buffer with key entity_name.

  "! <p class="shorttext synchronized" lang="en">Read from transaction buffer double</p>
  "!
  "! @parameter root_name | <p class="shorttext synchronized" lang="en">Root entity name</p>
  "! @parameter retrievals | <p class="shorttext synchronized" lang="en">List of operations</p>
  "! @parameter failed | <p class="shorttext synchronized" lang="en">The failed operations</p>
  "! @parameter reported | <p class="shorttext synchronized" lang="en">The messages</p>
  methods read
    importing root_name  type abp_root_entity_name
              retrievals type abp_behv_retrievals_tab
    changing  failed     type data
              reported   type data.

  "! <p class="shorttext synchronized" lang="en">Modify transactional buffer double</p>
  "!
  "! @parameter root_name | <p class="shorttext synchronized" lang="en">Root entity name</p>
  "! @parameter changes | <p class="shorttext synchronized" lang="en">List of operations</p>
  "! @parameter failed | <p class="shorttext synchronized" lang="en">The failed operations</p>
  "! @parameter mapped | <p class="shorttext synchronized" lang="en">The mapped keys</p>
  "! @parameter reported | <p class="shorttext synchronized" lang="en">The messages</p>
  methods modify
    importing root_name type abp_root_entity_name
              changes   type abp_behv_changes_tab
    changing  failed    type data
              mapped    type data
              reported  type data.

 "!<p class="shorttext synchronized" lang="en"> Configure response for MODIFY EML operations</p>
 methods config_response_4_modify default ignore
        importing root_name type abp_root_entity_name
                  operation type abp_behv_op_modify
                  mapped    type data optional
                  reported  type data optional
                  failed    type data optional
        raising
                  zcx_bo_tdf_failure.

 "!<p class="shorttext synchronized" lang="en">Configure response for READ EML operations</p>
 methods config_response_4_read default ignore
        importing root_name type abp_root_entity_name
                  operation type abp_behv_op_read
                  result    type standard table optional
                  reported  type data optional
                  failed    type data optional
        raising
                  zcx_bo_tdf_failure.

 "!<p class="shorttext synchronized" lang="en">Clearing the double transanctional buffer</p>
 methods clear_buffer.

endinterface.
