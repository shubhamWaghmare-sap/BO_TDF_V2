"! <p class="shorttext synchronized" lang="en">ABAP Behavior Test Transactional Buffer Double</p>
"!
"! <p> Interface to access the double transactional buffer used by generic test double for abap behavior test environment</p>
interface zif_abap_behv_test_trans_bufr
  public .

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
 "!<p class="shorttext synchronized" lang="en">Trying out!!! Configure reported and failed for modify</p>
 methods config_error_response_4_modify default ignore
        importing root_name type abp_root_entity_name
                   reported type data
                   failed   type data.

  "!<p class="shorttext synchronized" lang="en">Trying out!!! Configure reported  and failed for read</p>
 methods config_error_response_4_read default ignore
        importing root_name type abp_root_entity_name
                   reported type data
                   failed   type data .

 "!<p class="shorttext synchronized" lang="en">Trying out!!! Configure response for modify</p>
 methods config_response_4_modify default ignore
        importing root_name type abp_root_entity_name
                  operation type if_abap_behv=>t_char01
                  mapped    type data
                  reported  type data
                  failed    type data.


endinterface.
