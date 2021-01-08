class zcl_abap_behv_test_trans_bufr5 definition public final create private.

  public section.
    interfaces zif_abap_behv_test_trans_bufr5.

    class-methods:
      "! <p class="shorttext synchronized" lang="en">Get the transactional buffer instance</p>
      get_instance returning value(double_transactional_buffer) type ref to zif_abap_behv_test_trans_bufr5.
  protected section.
  private section.

    "Structure to hold bdef load attributes
    types:
      begin of ty_modify_response_config,
        operation type abp_behv_op_modify,
        mapped    type ref to data,
        reported  type ref to data,
        failed    type ref to data,
      end of   ty_modify_response_config.

    types:
      begin of ty_read_response_config,
        operation type if_abap_behv=>t_char01,
        result    type ref to data,
        reported  type ref to data,
        failed    type ref to data,
      end of   ty_read_response_config.

    types:
      begin of t_bdef_load,
        head         type cl_abap_behv_load=>t_head,
        entities     type cl_abap_behv_load=>tt_entity,
        features     type cl_abap_behv_load=>tt_feature,
        actions      type cl_abap_behv_load=>tt_action,
        relations    type cl_abap_behv_load=>tt_relat,
        associations type cl_abap_behv_load=>tt_assoc,
      end of t_bdef_load .

    types: begin of ty_double_transactional_buffer,
             entity_root      type abp_root_entity_name,
             entity_name      type abp_entity_name,
             entity_alias     type abp_entity_name,
             entity_instances type ref to data,
             reported         type ref to data,
             failed           type ref to data,
             modify_response_config type standard table of ty_modify_response_config with non-unique default key,
             read_response_config type standard table of ty_read_response_config with non-unique default key,
           end of ty_double_transactional_buffer.


    class-data:
     "! <p class="shorttext synchronized" lang="en">Transactional buffer double singleton instance</p>
     double_transactional_buffer type ref to zif_abap_behv_test_trans_bufr5.

    data:
     "! <p class="shorttext synchronized" lang="en">Transactional buffer double</p>
      dbl_transactional_buffer_table type standard table of ty_double_transactional_buffer with key entity_name.    " TODO : should we have a sorted table on entity name for binary search, or provide hashing to improve the search


    constants: begin of ty_op_category,
                modify type if_abap_behv=>t_char01 value 'm',
                read type if_abap_behv=>t_char01 value 'r',
               end of ty_op_category.

    constants: c_message_class type symsgid value 'ZBO_MOCKING_MESSAGE'.

    methods:
      check_instance_equal
        importing
                  instance_1    type any
                  instance_2    type any
                  entity_fields type ddfields
        returning value(equal)  type abap_bool,

      check_instance_exists
        importing
                  instance_to_check type any
                  entity_instances  type any table
                  entity_fields     type ddfields
        returning value(exists)     type abap_bool,

      get_entity_fields
        importing
                  entity_name          type abp_entity_name
        returning value(entity_fields) type ddfields,

      deep_clone
        importing
          data_to_be_cloned        type any
        exporting
          reference_to_cloned_data type ref to data,

      format_res_as_per_cntrl_flds
        importing
          control_fields  type any
          entity_fields   type ddfields
        changing
          entity_instance type any,

      fill_read_reported_and_failed
        importing
          i_entity_fields       type ddfields
          alias_name            type abp_entity_name
          message_number        type symsgno
          retrieval             type abp_behv_retrievals
        changing
          c_failed              type data
          c_reported            type data
          cs_retrieval_instance type any,

      fill_modify_reported_n_failed
        importing
          i_entity_fields    type ddfields
          alias_name         type abp_entity_name
          message_number     type symsgno
          change             type abp_behv_changes
        changing
          c_failed           type data
          c_reported         type data
          cs_change_instance type any,

      fill_modify_mapped
        importing
          i_entity_fields    type ddfields
          alias_name         type abp_entity_name
          message_number     type symsgno
          change             type abp_behv_changes
        changing
          c_mapped           type data
          cs_change_instance type any,

      _create
        importing
          change    type abp_behv_changes
          bdef_load type t_bdef_load
        changing
          failed    type data
          mapped    type data
          reported  type data,

      _delete
        importing
          change    type abp_behv_changes
          bdef_load type t_bdef_load
        changing
          failed    type data
          mapped    type data
          reported  type data,

      cascade_delete
        importing
          instance     type any
          associations type cl_abap_behv_load=>tt_assoc
          entity       type abp_entity_name,

      _update
        importing
          change    type abp_behv_changes
          bdef_load type t_bdef_load
        changing
          failed    type data
          mapped    type data
          reported  type data,


      _create_by_assoc
        importing
          change    type abp_behv_changes
          bdef_load type t_bdef_load
        changing
          failed    type data
          mapped    type data
          reported  type data,

      _read
        importing
          retrieval type abp_behv_retrievals
          bdef_load type t_bdef_load
        changing
          failed    type data
          reported  type data,

      _read_by_assoc
        importing
          retrieval type abp_behv_retrievals
          bdef_load type t_bdef_load
        changing
          failed    type data
          reported  type data,

      find_reported_if_configured
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_reported        type data
        returning
          value(result)     type abap_bool,

      find_reported_config_for_read
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_reported        type data
        returning
          value(result)     type abap_bool,

      find_reported_config_for_rba
        importing
          parent_entity     type cl_abap_behv_load=>t_entity
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_reported        type data
        returning
          value(result)     type abap_bool,

      find_failed_if_configured
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_failed          type data
        returning
          value(result)     type abap_bool,

      find_failed_config_for_read
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_failed          type data
        returning
          value(result)     type abap_bool,

      find_failed_config_for_rba
        importing
          parent_entity     type cl_abap_behv_load=>t_entity
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_failed          type data
        returning
          value(result)     type abap_bool,


      find_result_config_for_read
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_result          type standard table
        returning
          value(result)     type abap_bool,

      find_result_config_for_rba
        importing
          parent_entity     type cl_abap_behv_load=>t_entity
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_result          type standard table
        returning
          value(result)     type abap_bool,

      find_mapped_if_configured
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_mapped          type data
        returning
          value(result)     type abap_bool
                    .

    methods : configure_mapped
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        mapped    type data.

    methods : configure_reported
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        reported  type data.

    methods : configure_failed_for_modify
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        failed  type data.

    methods : configure_result_for_read
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        result    type standard table.

    methods : configure_reported_for_read
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        reported  type data.

    methods : configure_failed_for_read
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        failed  type data.

    methods : configure_failed
      importing
        entity              type cl_abap_behv_load=>t_entity
        operation_category  type if_abap_behv=>t_char01
        operation           type if_abap_behv=>t_char01
        failed              type data.

    methods : get_entity_alias
      importing
        entity    type cl_abap_behv_load=>t_entity
      returning
        value(alias)  type string.

endclass.



class zcl_abap_behv_test_trans_bufr5 implementation.


  method get_instance.
    if zcl_abap_behv_test_trans_bufr5=>double_transactional_buffer is initial.
      zcl_abap_behv_test_trans_bufr5=>double_transactional_buffer = new zcl_abap_behv_test_trans_bufr5( ).
    endif.
    double_transactional_buffer = zcl_abap_behv_test_trans_bufr5=>double_transactional_buffer.
  endmethod.


  method zif_abap_behv_test_trans_bufr5~read.

    data bdef_load type t_bdef_load.
    field-symbols <fs_retrieval> type abp_behv_retrievals.

    cl_abap_behv_load=>get_load(
                     exporting
                       entity   = root_name
                       all      = abap_true
                     importing
                       head         = bdef_load-head
                       entities     = bdef_load-entities
                       features     = bdef_load-features
                       actions      = bdef_load-actions
                       relations    = bdef_load-relations
                       associations = bdef_load-associations ).

    loop at retrievals assigning <fs_retrieval>.
      case <fs_retrieval>-op.
        when if_abap_behv=>op-r-read.
          _read(
            exporting
              retrieval = <fs_retrieval>
              bdef_load = bdef_load
            changing
              failed    = failed
              reported  = reported
          ).
        when if_abap_behv=>op-r-read_ba.
          _read_by_assoc(
            exporting
              retrieval = <fs_retrieval>
              bdef_load = bdef_load
            changing
              failed    = failed
              reported  = reported
          ).
      endcase.
    endloop.
  endmethod.


  method zif_abap_behv_test_trans_bufr5~modify.



    " steps

    " loop through each entity in the changes

    " check whether a buffer entry is present in the transactional buffer or not for the entity

    " if buffer entry is not present, first create a new buffer entry for the entity

    " If operation is CREATE

    "   loop through instances of entity to be created from the change

    "   for each instance to create, check whether the buffer entry for the entity already has an entry with same keys

    "   if entry is already present fill the failed and reported structure

    "   if instance is not present add the instance to the buffer entry and fill the mapped structure

    " if operation is UPDATE

    "   check whether the buffer contains an entry with the same primary keys, if present update the instance and fill mapped

    "   if not available, fill failed and reported

    " if operation is DELETE

    "   check whether the buffer contains an entry with the same primary keys, if present delete the instance

    "   if not available, fill failed and reported



    data bdef_load type t_bdef_load.

    cl_abap_behv_load=>get_load(
                     exporting
                       entity   = root_name
                       all      = abap_true
                     importing
                       head         = bdef_load-head
                       entities     = bdef_load-entities
                       features     = bdef_load-features
                       actions      = bdef_load-actions
                       relations    = bdef_load-relations
                       associations = bdef_load-associations ).


    loop at changes assigning field-symbol(<fs_change>).
      case <fs_change>-op.
        when if_abap_behv=>op-m-create.
          _create(
            exporting
              change    = <fs_change>
              bdef_load = bdef_load
            changing
              failed    = failed
              mapped    = mapped
              reported  = reported
          ).
        when if_abap_behv=>op-m-update.
          _update(
            exporting
              change    = <fs_change>
              bdef_load = bdef_load
            changing
              failed    = failed
              mapped    = mapped
              reported  = reported
          ).
        when if_abap_behv=>op-m-delete.
          _delete(
            exporting
              change    = <fs_change>
              bdef_load = bdef_load
            changing
              failed    = failed
              mapped    = mapped
              reported  = reported
          ).

        when if_abap_behv=>op-m-create_ba.
          _create_by_assoc(
            exporting
              change    = <fs_change>
              bdef_load = bdef_load
            changing
              failed    = failed
              mapped    = mapped
              reported  = reported
          ).

      endcase.
    endloop.
  endmethod.


  method check_instance_equal.
    data entity_field type dfies.
    field-symbols <instance_1_field> type any.
    field-symbols <instance_2_field> type any.

    loop at entity_fields into entity_field where keyflag = abap_true.
      assign component entity_field-fieldname of structure instance_1 to <instance_1_field>.
      assign component entity_field-fieldname of structure instance_2 to <instance_2_field>.
      if <instance_1_field> <> <instance_2_field>.
        " entity does not match
        equal = abap_false.
        return.
      endif.
    endloop.

    equal = abap_true.
    return.
  endmethod.


  method check_instance_exists.
    field-symbols <entity_instance> type any.

    loop at entity_instances assigning <entity_instance>.
      data(equal) = check_instance_equal(
                          instance_1    = instance_to_check
                          instance_2    = <entity_instance>
                          entity_fields = entity_fields ).

      if equal = abap_true.
        exists = abap_true.
        return.
      endif.
    endloop.

    exists = abap_false.
  endmethod.


  method get_entity_fields.
    data struct_descr type ref to cl_abap_structdescr.

    struct_descr ?= cl_abap_typedescr=>describe_by_name( entity_name ).
    entity_fields = struct_descr->get_ddic_field_list( ).
  endmethod.


  method deep_clone.
    field-symbols <fs_clone> type any.
    create data reference_to_cloned_data like data_to_be_cloned.
    assign reference_to_cloned_data->* to <fs_clone>.
    <fs_clone> = data_to_be_cloned.
  endmethod.


  method format_res_as_per_cntrl_flds.
    data entity_field type dfies.

    field-symbols <fs_control_field_value> type any.
    field-symbols <fs_data_to_be_removed> type any.

    loop at entity_fields into entity_field where fieldname <> '.NODE1' and keyflag = abap_false.
      assign component entity_field-fieldname of structure control_fields to <fs_control_field_value>.
      if <fs_control_field_value> = if_abap_behv=>mk-off.
        " clear data
        assign component entity_field-fieldname of structure entity_instance to <fs_data_to_be_removed>.
        clear <fs_data_to_be_removed>.
      endif.
    endloop.
  endmethod.


  method fill_read_reported_and_failed.

    field-symbols <fs_entry_failed_table> type standard table.
    field-symbols <fs_entry_reported_table> type standard table.

    assign component alias_name of structure c_failed to <fs_entry_failed_table>.
    data entry_failed type ref to data.
    create data entry_failed like line of <fs_entry_failed_table>.
    assign entry_failed->* to field-symbol(<fs_entry_failed>).
    "Assign key fields
    loop at i_entity_fields into data(entity_field) where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_retrieval_instance to field-symbol(<fs_val_1>).
      assign component entity_field-fieldname of structure <fs_entry_failed> to field-symbol(<fs_val_2>).
      <fs_val_2> = <fs_val_1>.
    endloop.
    "Assign fail reason
    assign component  cl_abap_behv=>co_techfield_name-fail of structure <fs_entry_failed> to field-symbol(<fail>).
    assign component 'CAUSE' of structure <fail> to field-symbol(<fail_reason>).
    <fail_reason> = if_abap_behv=>cause-not_found.
    "Assign Association if read by association operation.
    if retrieval-op eq if_abap_behv=>op-r-read_ba.
      assign component  cl_abap_behv=>co_techfield_name-assoc of structure <fs_entry_failed> to field-symbol(<assoc>).
      assign component  retrieval-sub_name of structure <assoc> to field-symbol(<assoc_subname>).
      <assoc_subname> = '01'.
    endif.

    append <fs_entry_failed> to <fs_entry_failed_table>.

    "Fill reported
    assign component alias_name of structure c_reported to <fs_entry_reported_table>.
    data entry_reported type ref to data.
    create data entry_reported like line of <fs_entry_reported_table>.
    assign entry_reported->* to field-symbol(<fs_entry_reported>).
    "Assign key fields
    loop at i_entity_fields into entity_field where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_retrieval_instance to <fs_val_1>.
      assign component entity_field-fieldname of structure <fs_entry_reported> to <fs_val_2>.
      <fs_val_2> = <fs_val_1>.
    endloop.

    "Assign exception message instance
    data(lo_message) = new lcl_exception_handler( )->new_message( id = c_message_class
                                   number   = message_number
                                   severity = if_abap_behv_message=>severity-error ).

    field-symbols:<fs_message_obj> type ref to if_abap_behv_message.
    field-symbols:<fs_message_obj2> type ref to if_abap_behv_message.
    assign component cl_abap_behv=>co_techfield_name-msg of structure <fs_entry_reported> to <fs_message_obj2>.
    assign  lo_message to <fs_message_obj>.
    <fs_message_obj2> = <fs_message_obj>.
    append <fs_entry_reported> to <fs_entry_reported_table>.

  endmethod.


  method fill_modify_reported_n_failed.

    field-symbols <fs_entry_failed_table> type standard table.
    field-symbols <fs_entry_reported_table> type standard table.

    assign component alias_name of structure c_failed to <fs_entry_failed_table>.
    data entry_failed type ref to data.
    create data entry_failed like line of <fs_entry_failed_table>.
    assign entry_failed->* to field-symbol(<fs_entry_failed>).
    "Assign key fields
    loop at i_entity_fields into data(entity_field) where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_change_instance to field-symbol(<fs_val_1>).
      assign component entity_field-fieldname of structure <fs_entry_failed> to field-symbol(<fs_val_2>).
      <fs_val_2> = <fs_val_1>.
    endloop.
    "Assign fail reason
    assign component  cl_abap_behv=>co_techfield_name-fail of structure <fs_entry_failed> to field-symbol(<fail>).
    assign component 'CAUSE' of structure <fail> to field-symbol(<fail_reason>).

    " Make operation specific additions to failed
    case change-op.
      when if_abap_behv=>op-m-create.
        <fail_reason> = if_abap_behv=>cause-unspecific.
        assign component  cl_abap_behv=>co_techfield_name-create of structure <fs_entry_failed> to field-symbol(<create>).
        <create> = '01'.

      when if_abap_behv=>op-m-create_ba.
        <fail_reason> = if_abap_behv=>cause-unspecific.
        assign component  cl_abap_behv=>co_techfield_name-create of structure <fs_entry_failed> to <create>.
        <create> = '01'.

      when if_abap_behv=>op-m-update.
        <fail_reason> = if_abap_behv=>cause-not_found.
        assign component  cl_abap_behv=>co_techfield_name-update of structure <fs_entry_failed> to field-symbol(<update>).
        <update> = '01'.

      when if_abap_behv=>op-m-delete.
        <fail_reason> = if_abap_behv=>cause-not_found.
        assign component  cl_abap_behv=>co_techfield_name-delete of structure <fs_entry_failed> to field-symbol(<delete>).
        <delete> = '01'.

    endcase.

    append <fs_entry_failed> to <fs_entry_failed_table>.

    "Fill reported
    assign component alias_name of structure c_reported to <fs_entry_reported_table>.
    data entry_reported type ref to data.
    create data entry_reported like line of <fs_entry_reported_table>.
    assign entry_reported->* to field-symbol(<fs_entry_reported>).
    "Assign key fields
    loop at i_entity_fields into entity_field where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_change_instance to <fs_val_1>.
      assign component entity_field-fieldname of structure <fs_entry_reported> to <fs_val_2>.
      <fs_val_2> = <fs_val_1>.
    endloop.

    "Assign exception message instance
    data(lo_message) = new lcl_exception_handler( )->new_message( id = c_message_class
                                   number   = message_number
                                   severity = if_abap_behv_message=>severity-error ).

    field-symbols:<fs_message_obj> type ref to if_abap_behv_message.
    field-symbols:<fs_message_obj2> type ref to if_abap_behv_message.
    assign component cl_abap_behv=>co_techfield_name-msg of structure <fs_entry_reported> to <fs_message_obj2>.
    assign  lo_message to <fs_message_obj>.
    <fs_message_obj2> = <fs_message_obj>.
    append <fs_entry_reported> to <fs_entry_reported_table>.

  endmethod.


  method fill_modify_mapped.

    field-symbols <fs_entry_mapped_table> type standard table.

    assign component alias_name of structure c_mapped to <fs_entry_mapped_table>.
    data entry_mapped type ref to data.
    create data entry_mapped like line of <fs_entry_mapped_table>.
    assign entry_mapped->* to field-symbol(<fs_entry_mapped>).

    move-corresponding cs_change_instance to <fs_entry_mapped>.

    "Assign key fields
    loop at i_entity_fields into data(entity_field) where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_change_instance to field-symbol(<fs_val_1>).
      assign component entity_field-fieldname of structure <fs_entry_mapped> to field-symbol(<fs_val_2>).
      <fs_val_2> = <fs_val_1>.
    endloop.

    append <fs_entry_mapped> to <fs_entry_mapped_table>.

  endmethod.

  method cascade_delete.

    data buffer_4_assoc_entity type ty_double_transactional_buffer.
    data buffer_4_entity type ty_double_transactional_buffer.
    data entity_buffer_instance type ref to data.

    data(entity_fields) = get_entity_fields( entity ).

    read table associations into data(association) with key source_entity = entity.
    if sy-subrc eq 0.
      data(associated_entity) = association-target_entity.
    endif.

    if associated_entity is not initial.
      if line_exists( dbl_transactional_buffer_table[ entity_name = associated_entity  ] ).
        buffer_4_assoc_entity = dbl_transactional_buffer_table[ entity_name = associated_entity  ].
      endif.

      if buffer_4_assoc_entity is not initial.
        field-symbols <assoc_entity_buff_instances> type standard table.
        assign buffer_4_assoc_entity-entity_instances->* to <assoc_entity_buff_instances>.
        if <assoc_entity_buff_instances> is assigned.
          loop at <assoc_entity_buff_instances> assigning field-symbol(<assoc_entity_buffer_instance>).

            "check if the instance is associated with the input entity instance
            data(equal) = check_instance_equal(
                                exporting
                                  instance_1    = <assoc_entity_buffer_instance>
                                  instance_2    = instance
                                  entity_fields = entity_fields    " here we are passing the entity fields of parent entity and not associated entity because
                                                                    "  the read happens only based on the key value of parent and not associated entity
                            ).

            if equal eq abap_true.
              cascade_delete( associations = associations entity = associated_entity instance = <assoc_entity_buffer_instance>  ).
            endif.

          endloop.
        endif.
      endif.
    endif.

    "delete the instance
    if line_exists( dbl_transactional_buffer_table[ entity_name = entity  ] ).
      buffer_4_entity = dbl_transactional_buffer_table[ entity_name = entity  ].

      if buffer_4_entity is not initial.
        field-symbols <instances_4_entity_in_buffer> type standard table.
        assign buffer_4_entity-entity_instances->* to <instances_4_entity_in_buffer>.
        " TODO: Check if an assignment check is required.
        loop at <instances_4_entity_in_buffer> assigning field-symbol(<instance>).
          "look for a matching instance to be deleted
          equal = check_instance_equal(
                    exporting
                       instance_1    = <instance>
                       instance_2    = instance
                       entity_fields = entity_fields
                       ).

          if equal eq abap_true.
            delete <instances_4_entity_in_buffer>.
            exit.
          endif.
        endloop.

      endif.

    endif.

  endmethod.

  method _create_by_assoc.
    data buffer_4_entity type ty_double_transactional_buffer.
    data entity_buffer_instance type ref to data.
    data struct_descr_4_entity type ref to cl_abap_structdescr.
    data entity_fields type ddfields.

    field-symbols <instances_4_entity_in_buffer> type standard table.
    field-symbols <input_entity_instances> type standard table.
    field-symbols <input_entity_instance> type any.
    field-symbols <entity_buffer_instance> type any.

    if line_exists( dbl_transactional_buffer_table[ entity_name = change-entity_name ] ).
      buffer_4_entity = dbl_transactional_buffer_table[ entity_name = change-entity_name ].
    endif.

    if buffer_4_entity is initial.
      " no buffer entry present for the entity
      buffer_4_entity-entity_name = change-entity_name.
      buffer_4_entity-entity_instances = cl_abap_behvdescr=>create_data(
                                                      p_name     = change-entity_name
                                                      p_op       = if_abap_behv=>op-r-read "TODO : Should it be modify/read?
                                                      p_kind     = if_abap_behv=>typekind-result
                                                 ).
      append buffer_4_entity to dbl_transactional_buffer_table.
    endif.

    struct_descr_4_entity ?= cl_abap_typedescr=>describe_by_name( change-entity_name ).
    entity_fields = struct_descr_4_entity->get_ddic_field_list( ).

    data(entity_details) = bdef_load-entities[ name = change-entity_name ].
    data(alias)  = entity_details-alias.
    alias = to_upper( alias ).
    if alias is not initial.
      buffer_4_entity-entity_alias = alias.
    else.
      buffer_4_entity-entity_alias = change-entity_name.
    endif. "Won't be filled in all the use cases.

    assign buffer_4_entity-entity_instances->* to <instances_4_entity_in_buffer>.
    assign change-instances->* to <input_entity_instances>.

    loop at <input_entity_instances> assigning <input_entity_instance>.
      "check whether the entity instance is already present in the buffer
      data(instance_exists_in_buffer) = check_instance_exists(
                         exporting
                           instance_to_check = <input_entity_instance>
                           entity_instances  = <instances_4_entity_in_buffer>
                           entity_fields     = entity_fields
                           ).

      data buffer_4_assoc_entity type ty_double_transactional_buffer.
      data struct_descr_assoc_entity type ref to cl_abap_structdescr.
      data entity_fields_of_assoc_entity type ddfields.

      field-symbols <input_assoc_entity_instances> type standard table.
      field-symbols <input_assoc_entity_instance> type any.

      data(association) = bdef_load-associations[ source_entity =  change-entity_name ].
      data(associated_entity) = association-target_entity.

      struct_descr_assoc_entity  ?= cl_abap_typedescr=>describe_by_name( associated_entity ).
      entity_fields_of_assoc_entity = struct_descr_assoc_entity->get_ddic_field_list( ).

      assign component cl_abap_behv=>co_techfield_name-target
        of structure <input_entity_instance> to <input_assoc_entity_instances>.

      loop at <input_assoc_entity_instances> assigning <input_assoc_entity_instance>.
        move-corresponding <input_entity_instance> to <input_assoc_entity_instance>.
        " make sure the foreign key values are filled in the associated entity instance
        loop at bdef_load-relations[ source_entity = associated_entity ]-foreignkeys into data(key).
          assign component key-local_name of structure <input_entity_instance> to field-symbol(<fs_val_1>).
          assign component key-local_name of structure <input_assoc_entity_instance> to field-symbol(<fs_val_2>).
          <fs_val_2> = <fs_val_1>.
        endloop.

        " Check if buffer entry exists for the associated entity
        if line_exists( dbl_transactional_buffer_table[ entity_name = associated_entity ] ).
          buffer_4_assoc_entity = dbl_transactional_buffer_table[ entity_name = associated_entity  ].
        endif.

        if  buffer_4_assoc_entity  is initial.
          " no buffer entry present for the associated entity
          buffer_4_assoc_entity-entity_name = associated_entity.
          buffer_4_assoc_entity-entity_instances = cl_abap_behvdescr=>create_data(
                                                    p_name     = associated_entity
                                                    p_op       = if_abap_behv=>op-r-read
                                                    p_kind     = if_abap_behv=>typekind-result

                                                   ).
          append buffer_4_assoc_entity to dbl_transactional_buffer_table.
        endif.

        " evaluate the alias name for associated entity
        data(assoc_entity_details) = bdef_load-entities[ name = associated_entity ].
        data(assoc_entity_alias) = assoc_entity_details-alias.
        assoc_entity_alias = to_upper( assoc_entity_alias ).

        if assoc_entity_alias is not initial.
          buffer_4_assoc_entity-entity_alias = assoc_entity_alias.
        else.
          assoc_entity_alias = associated_entity.
          buffer_4_assoc_entity-entity_alias = associated_entity.
        endif. "Won't be filled in all the use cases.

        " if the parent entity instance itself doesn't exist
        " fill mapped and reported. No further processing required
        if instance_exists_in_buffer = abap_false.
          " fill failed and reported
          fill_modify_reported_n_failed(
            exporting
              i_entity_fields = entity_fields_of_assoc_entity
              alias_name = assoc_entity_alias
              message_number = 001
              change = change
            changing
              c_failed = failed
              c_reported = reported
              cs_change_instance = <input_assoc_entity_instance> ).

          continue.
        endif.

        field-symbols <assoc_entity_buff_instances> type standard table.
        assign buffer_4_assoc_entity-entity_instances->* to <assoc_entity_buff_instances>.

        "check whether the associated entity instance is already present in the buffer
        data(assoc_instance_exists_in_buff) = check_instance_exists(
                            exporting
                              instance_to_check = <input_assoc_entity_instance>
                              entity_instances  = <assoc_entity_buff_instances>
                              entity_fields     = entity_fields_of_assoc_entity
                         ).

        if assoc_instance_exists_in_buff = abap_true.

        data(failed_configured) = find_failed_if_configured(
                                      exporting
                                        entity            = assoc_entity_details
                                        instance_to_check = <input_assoc_entity_instance>
                                        operation         = if_abap_behv=>op-m-create_ba
                                      changing
                                        buffer_4_entity   = buffer_4_assoc_entity
                                        c_failed          = failed
                                    ).

        data(reported_configured) = find_reported_if_configured(
                                      exporting
                                        entity            = assoc_entity_details
                                        instance_to_check = <input_assoc_entity_instance>
                                        operation         = if_abap_behv=>op-m-create_ba
                                      changing
                                        buffer_4_entity   = buffer_4_assoc_entity
                                        c_reported        = reported
                                    ).

        data(mapped_configured) = find_mapped_if_configured(
                                      exporting
                                        entity            = assoc_entity_details
                                        instance_to_check = <input_assoc_entity_instance>
                                        operation         = if_abap_behv=>op-m-create_ba
                                      changing
                                        buffer_4_entity   = buffer_4_assoc_entity
                                        c_mapped          = mapped
                                    ).

        " fill failed and reported
        if reported_configured eq abap_false and mapped_configured eq abap_false and failed_configured eq abap_false.
          " fill failed and reported
          fill_modify_reported_n_failed(
            exporting
              i_entity_fields = entity_fields_of_assoc_entity
              alias_name = assoc_entity_alias
              message_number = 001
              change = change
            changing
              c_failed = failed
              c_reported = reported
              cs_change_instance = <input_assoc_entity_instance> ).

        endif.


        else.
          data assoc_entity_buffer_instance type ref to data.
          field-symbols <assoc_entity_buffer_instance> type any.

          create data assoc_entity_buffer_instance like line of <assoc_entity_buff_instances>.
          assign assoc_entity_buffer_instance->* to <assoc_entity_buffer_instance>.
          <assoc_entity_buffer_instance> = corresponding #( <input_assoc_entity_instance> ).
          append <assoc_entity_buffer_instance> to <assoc_entity_buff_instances>.

          " Fill mapped table
          fill_modify_mapped(
            exporting
             alias_name = assoc_entity_alias
             i_entity_fields = entity_fields_of_assoc_entity
             message_number = '001'
             change = change
            changing
              c_mapped = mapped
              cs_change_instance = <assoc_entity_buffer_instance> ).
        endif.
      endloop.
    endloop.

  endmethod.

  method _create.

    data buffer_4_entity type ty_double_transactional_buffer.
    data entity_buffer_instance type ref to data.
    data struct_descr_4_entity type ref to cl_abap_structdescr.
    data entity_fields type ddfields.

    field-symbols <instances_4_entity_in_buffer> type standard table.
    field-symbols <input_entity_instances> type standard table.
    field-symbols <input_entity_instance> type any.
    field-symbols <entity_buffer_instance> type any.

    if line_exists( dbl_transactional_buffer_table[ entity_name = change-entity_name ] ).
      buffer_4_entity = dbl_transactional_buffer_table[ entity_name = change-entity_name ].
    endif.

    if buffer_4_entity is initial.
      " no buffer entry present for the entity
      buffer_4_entity-entity_name = change-entity_name.
      buffer_4_entity-entity_instances = cl_abap_behvdescr=>create_data(
                                                      p_name     = change-entity_name
                                                      p_op       = if_abap_behv=>op-r-read "TODO : Should it be modify/read?
                                                      p_kind     = if_abap_behv=>typekind-result
                                                 ).
      append buffer_4_entity to dbl_transactional_buffer_table.
    endif.

    struct_descr_4_entity ?= cl_abap_typedescr=>describe_by_name( change-entity_name ).
    entity_fields = struct_descr_4_entity->get_ddic_field_list( ).

    data(entity_details) = bdef_load-entities[ name = change-entity_name ].
    data(alias)  = entity_details-alias.
    alias = to_upper( alias ).
    if alias is not initial.
      buffer_4_entity-entity_alias = alias.
    else.
      buffer_4_entity-entity_alias = change-entity_name.
    endif. "Won't be filled in all the use cases.

    assign buffer_4_entity-entity_instances->* to <instances_4_entity_in_buffer>.
    assign change-instances->* to <input_entity_instances>.

    loop at <input_entity_instances> assigning <input_entity_instance>.
      "check whether the entity instance is already present in the buffer
      data(instance_exists_in_buffer) = check_instance_exists(
                         exporting
                           instance_to_check = <input_entity_instance>
                           entity_instances  = <instances_4_entity_in_buffer>
                           entity_fields     = entity_fields
                           ).

      if instance_exists_in_buffer = abap_true.

        data(failed_configured) = find_failed_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-create
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_failed          = failed
                                    ).

        data(reported_configured) = find_reported_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-create
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_reported        = reported
                                    ).

        data(mapped_configured) = find_mapped_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-create
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_mapped          = mapped
                                    ).

        " fill failed and reported
        if reported_configured eq abap_false and mapped_configured eq abap_false and failed_configured eq abap_false.
          fill_modify_reported_n_failed(
           exporting
            i_entity_fields = entity_fields
            alias_name = alias
            message_number = 001
            change = change
           changing
            c_failed = failed
            c_reported = reported
            cs_change_instance = <input_entity_instance> ).
        endif.

      else.

        " Create instance
        create data entity_buffer_instance like line of <instances_4_entity_in_buffer>.
        assign entity_buffer_instance->* to <entity_buffer_instance>.
        <entity_buffer_instance> = corresponding #( <input_entity_instance> ).
        append <entity_buffer_instance> to <instances_4_entity_in_buffer>.

        " Fill mapped table
        fill_modify_mapped(
          exporting
            alias_name = alias
            i_entity_fields = entity_fields
            message_number = '001'
            change = change
          changing
            c_mapped = mapped
            cs_change_instance = <input_entity_instance> ).

      endif.
    endloop.

  endmethod.

  method _delete.

    data buffer_4_entity type ty_double_transactional_buffer.
    data entity_buffer_instance type ref to data.
    data struct_descr_4_entity type ref to cl_abap_structdescr.
    data entity_fields type ddfields.

    field-symbols <instances_4_entity_in_buffer> type standard table.
    field-symbols <input_entity_instances> type standard table.
    field-symbols <input_entity_instance> type any.
    field-symbols <entity_buffer_instance> type any.

    if line_exists( dbl_transactional_buffer_table[ entity_name = change-entity_name ] ).
      buffer_4_entity = dbl_transactional_buffer_table[ entity_name = change-entity_name ].
    endif.

    if buffer_4_entity is initial.
      " no buffer entry present for the entity
      buffer_4_entity-entity_name = change-entity_name.
      buffer_4_entity-entity_instances = cl_abap_behvdescr=>create_data(
                                                      p_name     = change-entity_name
                                                      p_op       = if_abap_behv=>op-r-read "TODO : Should it be modify/read?
                                                      p_kind     = if_abap_behv=>typekind-result
                                                 ).
      append buffer_4_entity to dbl_transactional_buffer_table.
    endif.

    struct_descr_4_entity ?= cl_abap_typedescr=>describe_by_name( change-entity_name ).
    entity_fields = struct_descr_4_entity->get_ddic_field_list( ).

    data(entity_details) = bdef_load-entities[ name = change-entity_name ].
    data(alias)  = entity_details-alias.
    alias = to_upper( alias ).
    if alias is not initial.
      buffer_4_entity-entity_alias = alias.
    else.
      buffer_4_entity-entity_alias = change-entity_name.
    endif. "Won't be filled in all the use cases.

    assign buffer_4_entity-entity_instances->* to <instances_4_entity_in_buffer>.
    assign change-instances->* to <input_entity_instances>.

    loop at <input_entity_instances> assigning <input_entity_instance>.
      "check whether the entity instance is already present in the buffer
      data(instance_exists_in_buffer) = check_instance_exists(
                         exporting
                           instance_to_check = <input_entity_instance>
                           entity_instances  = <instances_4_entity_in_buffer>
                           entity_fields     = entity_fields
                           ).

      if instance_exists_in_buffer = abap_false.
        " fill failed and reported
        fill_modify_reported_n_failed(
         exporting
          i_entity_fields = entity_fields
          alias_name = alias
          message_number = 001
          change = change
        changing
          c_failed = failed
          c_reported = reported
          cs_change_instance = <input_entity_instance> ).

      else.

        data(failed_configured) = find_failed_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-delete
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_failed          = failed
                                    ).

        data(reported_configured) = find_reported_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-delete
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_reported        = reported
                                    ).

        data(mapped_configured) = find_mapped_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-delete
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_mapped        = mapped
                                    ).

        if reported_configured eq abap_false and mapped_configured eq abap_false and failed_configured eq abap_false.
          "delete the instance, along with all the associated instances
          cascade_delete( associations = bdef_load-associations entity = change-entity_name  instance = <input_entity_instance>  ).
        endif.
        "no mapped in this case.
      endif.
    endloop.
  endmethod.

  method _update.
    data buffer_4_entity type ty_double_transactional_buffer.
    data entity_buffer_instance type ref to data.
    data struct_descr_4_entity type ref to cl_abap_structdescr.
    data entity_fields type ddfields.

    field-symbols <instances_4_entity_in_buffer> type standard table.
    field-symbols <input_entity_instances> type standard table.
    field-symbols <input_entity_instance> type any.
    field-symbols <entity_buffer_instance> type any.

    if line_exists( dbl_transactional_buffer_table[ entity_name = change-entity_name ] ).
      buffer_4_entity = dbl_transactional_buffer_table[ entity_name = change-entity_name ].
    endif.

    if buffer_4_entity is initial.
      " no buffer entry present for the entity
      buffer_4_entity-entity_name = change-entity_name.
      buffer_4_entity-entity_instances = cl_abap_behvdescr=>create_data(
                                                      p_name     = change-entity_name
                                                      p_op       = if_abap_behv=>op-r-read "TODO : Should it be modify/read?
                                                      p_kind     = if_abap_behv=>typekind-result
                                                 ).
      append buffer_4_entity to dbl_transactional_buffer_table.
    endif.

    struct_descr_4_entity ?= cl_abap_typedescr=>describe_by_name( change-entity_name ).
    entity_fields = struct_descr_4_entity->get_ddic_field_list( ).

    data(entity_details) = bdef_load-entities[ name = change-entity_name ].
    data(alias)  = entity_details-alias.
    alias = to_upper( alias ).
    if alias is not initial.
      buffer_4_entity-entity_alias = alias.
    else.
      buffer_4_entity-entity_alias = change-entity_name.
    endif. "Won't be filled in all the use cases.

    assign buffer_4_entity-entity_instances->* to <instances_4_entity_in_buffer>.
    assign change-instances->* to <input_entity_instances>.

    loop at <input_entity_instances> assigning <input_entity_instance>.
      "check whether the entity instance is already present in the buffer
      data(instance_exists_in_buffer) = check_instance_exists(
                         exporting
                           instance_to_check = <input_entity_instance>
                           entity_instances  = <instances_4_entity_in_buffer>
                           entity_fields     = entity_fields
                           ).
      if instance_exists_in_buffer = abap_false.
        " fill failed and reported
        fill_modify_reported_n_failed(
         exporting
          i_entity_fields = entity_fields
          alias_name = alias
          message_number = 001
          change = change
        changing
          c_failed = failed
          c_reported = reported
          cs_change_instance = <input_entity_instance> ).

      else.

        data(failed_configured) = find_failed_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-update
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_failed          = failed
                                    ).

        data(reported_configured) = find_reported_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-update
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_reported        = reported
                                    ).

        data(mapped_configured) = find_mapped_if_configured(
                                      exporting
                                        entity            = entity_details
                                        instance_to_check = <input_entity_instance>
                                        operation         = if_abap_behv=>op-m-update
                                      changing
                                        buffer_4_entity   = buffer_4_entity
                                        c_mapped        = mapped
                                    ).


        " update the instance
        if reported_configured eq abap_false and mapped_configured eq abap_false and failed_configured eq abap_false.
          create data entity_buffer_instance like line of <instances_4_entity_in_buffer>.
          assign entity_buffer_instance->* to <entity_buffer_instance>.
          <entity_buffer_instance> = corresponding #( <input_entity_instance> ).
          modify table <instances_4_entity_in_buffer> from <entity_buffer_instance> .

          " fill mapped
          fill_modify_mapped(
            exporting
             alias_name = alias
             i_entity_fields = entity_fields
             message_number = '001'
             change = change
            changing
              c_mapped = mapped
              cs_change_instance = <input_entity_instance> ).
        endif.

      endif.
    endloop.

  endmethod.

  method _read.

    data entity_fields type ddfields.
    data buffer_entry_for_entity type ty_double_transactional_buffer.
    data entity_buffer_instance_copy type ref to data.

    field-symbols <fs_retrieval_instances> type standard table.
    field-symbols <fs_retrieval_instance> type any.
    field-symbols <fs_control_fields> type any.

    field-symbols <entity_buffer_instances> type standard table.
    field-symbols <entity_buffer_instance> type any.
    field-symbols <entity_buffer_instance_copy> type any.
    field-symbols <fs_result> type standard table.

    " retrieve the buffer entry for the entity
    if line_exists( dbl_transactional_buffer_table[ entity_name = retrieval-entity_name ] ).
      buffer_entry_for_entity = dbl_transactional_buffer_table[ entity_name = retrieval-entity_name ].
      loop at bdef_load-entities into data(entity) where name = retrieval-entity_name.
        buffer_entry_for_entity-entity_alias = entity-alias.
        exit.
      endloop.
    endif.

    " retrieve fields of the entity
    entity_fields = get_entity_fields( retrieval-entity_name ).
    " get the reference to the entity instances requested
    assign retrieval-instances->* to <fs_retrieval_instances>.
    " get the reference to the output result
    assign retrieval-results->* to <fs_result>.

    " get the reference to the entity instances in buffer
    assign buffer_entry_for_entity-entity_instances->* to <entity_buffer_instances>.

    " TODO: Fill failed structure when the entry does not exist in the transactional buffer
    if buffer_entry_for_entity is initial.
      "TODO: Throw exception that entity doesn't exist.
      data(entity_details) = retrieval-entity_name.
      loop at bdef_load-entities into entity where name = retrieval-entity_name.
        buffer_entry_for_entity-entity_alias = entity-alias.
        exit.
      endloop.
      data(alias) = buffer_entry_for_entity-entity_alias.
      alias = to_upper( alias ).
      loop at <fs_retrieval_instances> assigning <fs_retrieval_instance>.
        fill_read_reported_and_failed(
         exporting
          i_entity_fields = entity_fields
          alias_name = alias
          message_number = 001
          retrieval = retrieval
        changing
          c_failed = failed
          c_reported = reported
          cs_retrieval_instance = <fs_retrieval_instance> ).
      endloop.
      exit.
    endif.

    " TODO: Probably we can get rid of key fields calculation from the structure. Because the retrieval instance entry already
    " has all the key fields. So probably loop through the components and filter out the key fields.

*---- read the buffer entries corresponding to the entries in the retrieval table

    loop at <fs_retrieval_instances> assigning <fs_retrieval_instance>.

      " check whether the retrieval entry is present in the buffer
      loop at <entity_buffer_instances> assigning <entity_buffer_instance>.

        data(equal) = check_instance_equal(
                          exporting
                            instance_1    = <entity_buffer_instance>
                            instance_2    = <fs_retrieval_instance>
                            entity_fields = entity_fields
                      ).

        if equal = abap_true.

        data(failed_configured) = find_failed_config_for_read(
                                      exporting
                                        entity            = entity
                                        instance_to_check = <fs_retrieval_instance>
                                        operation         = if_abap_behv=>op-r-read
                                      changing
                                        buffer_4_entity   = buffer_entry_for_entity
                                        c_failed          = failed
                                    ).

        data(reported_configured) = find_reported_config_for_read(
                                      exporting
                                        entity            = entity
                                        instance_to_check = <fs_retrieval_instance>
                                        operation         = if_abap_behv=>op-r-read
                                      changing
                                        buffer_4_entity   = buffer_entry_for_entity
                                        c_reported        = reported
                                    ).

        data(result_configured) = find_result_config_for_read(
                                      exporting
                                        entity            = entity
                                        instance_to_check = <fs_retrieval_instance>
                                        operation         = if_abap_behv=>op-r-read
                                      changing
                                        buffer_4_entity   = buffer_entry_for_entity
                                        c_result          = <fs_result>
                                    ).

          if reported_configured eq abap_false and failed_configured eq abap_false and result_configured eq abap_false.
            " create a copy of buffer instance entry
            deep_clone( exporting data_to_be_cloned = <entity_buffer_instance> importing reference_to_cloned_data = entity_buffer_instance_copy ).
            assign entity_buffer_instance_copy->* to <entity_buffer_instance_copy>.

            " format data as per the control structures provided
            assign component '%control' of structure <fs_retrieval_instance> to <fs_control_fields>.
            format_res_as_per_cntrl_flds(
              exporting
                control_fields  = <fs_control_fields>
                entity_fields   = entity_fields
              changing
                entity_instance = <entity_buffer_instance_copy>
            ).

            append <entity_buffer_instance_copy> to <fs_result>.
            exit.

          endif.
        endif.

      endloop.

      "Instance doesn't exist in buffer
      if equal eq abap_false.

        entity_details = retrieval-entity_name.
        alias = buffer_entry_for_entity-entity_alias.
        alias = to_upper( alias ).

       fill_read_reported_and_failed(
         exporting
          i_entity_fields = entity_fields
          alias_name = alias
          message_number = 001
          retrieval = retrieval
        changing
          c_failed = failed
          c_reported = reported
          cs_retrieval_instance = <fs_retrieval_instance> ).
      endif.

    endloop.

  endmethod.

  method _read_by_assoc.

    data entity_fields type ddfields.
    data buffer_entry_for_entity type ty_double_transactional_buffer.
    data entity_buffer_instance_copy type ref to data.

    field-symbols <fs_retrieval_instances> type standard table.
    field-symbols <fs_retrieval_instance> type any.
    field-symbols <fs_control_fields> type any.

    field-symbols <entity_buffer_instances> type standard table.
    field-symbols <entity_buffer_instance> type any.
    field-symbols <entity_buffer_instance_copy> type any.
    field-symbols <fs_result> type standard table.

    " retrieve the buffer entry for the entity
    if line_exists( dbl_transactional_buffer_table[ entity_name = retrieval-entity_name ] ).
      buffer_entry_for_entity = dbl_transactional_buffer_table[ entity_name = retrieval-entity_name ].
      loop at bdef_load-entities into data(entity) where name = retrieval-entity_name.
        buffer_entry_for_entity-entity_alias = entity-alias.
        exit.
      endloop.
    endif.

    " retrieve fields of the entity
    entity_fields = get_entity_fields( retrieval-entity_name ).
    " get the reference to the entity instances requested
    assign retrieval-instances->* to <fs_retrieval_instances>.
    " get the reference to the output result
    assign retrieval-results->* to <fs_result>.

    " get the reference to the entity instances in buffer
    assign buffer_entry_for_entity-entity_instances->* to <entity_buffer_instances>.

    " TODO: Fill failed structure when the entry does not exist in the transactional buffer
    if buffer_entry_for_entity is initial.
      "TODO: Throw exception that entity doesn't exist.
      data(entity_details) = retrieval-entity_name.
      loop at bdef_load-entities into entity where name = retrieval-entity_name.
        buffer_entry_for_entity-entity_alias = entity-alias.
        exit.
      endloop.
      data(alias) = buffer_entry_for_entity-entity_alias.
      alias = to_upper( alias ).
      loop at <fs_retrieval_instances> assigning <fs_retrieval_instance>.
        fill_read_reported_and_failed(
         exporting
          i_entity_fields = entity_fields
          alias_name = alias
          message_number = 001
          retrieval = retrieval
        changing
          c_failed = failed
          c_reported = reported
          cs_retrieval_instance = <fs_retrieval_instance> ).
      endloop.
      exit.
    endif.

    " TODO: Probably we can get rid of key fields calculation from the structure. Because the retrieval instance entry already
    " has all the key fields. So probably loop through the components and filter out the key fields.

*---- read the associated entity buffer entries corresponding to the entries in the retrieval table

    data(association) = bdef_load-associations[ name =  retrieval-sub_name ].
    data(associated_entity) = association-target_entity.
    data buffer_entry_for_assoc_entity type ty_double_transactional_buffer.
    " retrieve fields of the associated entity
    data(assoc_entity_fields) = get_entity_fields( associated_entity ).

    loop at <fs_retrieval_instances> assigning <fs_retrieval_instance>.

      " check whether the retrieval entry is present in the buffer
      loop at <entity_buffer_instances> assigning <entity_buffer_instance>.

        data(parent_equal) = check_instance_equal(
                          exporting
                            instance_1    = <entity_buffer_instance>
                            instance_2    = <fs_retrieval_instance>
                            entity_fields = entity_fields
                      ).

        if parent_equal = abap_true.
          " retrieve the buffer entry for the associated entity
          if line_exists( dbl_transactional_buffer_table[ entity_name = associated_entity ] ).
            buffer_entry_for_assoc_entity = dbl_transactional_buffer_table[ entity_name = associated_entity ].
            loop at bdef_load-entities into entity where name = associated_entity.
              buffer_entry_for_assoc_entity-entity_alias = entity-alias.
              exit.
            endloop.
          endif.

          field-symbols <assoc_entity_buff_instances> type standard table.
          assign buffer_entry_for_assoc_entity-entity_instances->* to <assoc_entity_buff_instances>.

          " check whether the retrieval entry is present in the buffer
          if <assoc_entity_buff_instances> is assigned.
            loop at <assoc_entity_buff_instances> assigning field-symbol(<assoc_entity_buffer_instance>).

              data(equal) = check_instance_equal(
                          exporting
                            instance_1    = <assoc_entity_buffer_instance>
                            instance_2    = <fs_retrieval_instance>
                            entity_fields =  entity_fields    " here we are passing the entity fields of parent entity and not associated entity because
                                                              "  the read happens only based on the key value of parent and not associated entity
                      ).

              if equal = abap_true.

                data(failed_configured) = find_failed_config_for_rba(
                                              exporting
                                                parent_entity     = bdef_load-entities[ name = association-source_entity ]
                                                entity            = entity
                                                instance_to_check = <fs_retrieval_instance>
                                                operation         = if_abap_behv=>op-r-read_ba
                                              changing
                                                buffer_4_entity   = buffer_entry_for_assoc_entity
                                                c_failed          = failed
                                            ).

                data(reported_configured) = find_reported_config_for_rba(
                                              exporting
                                                parent_entity     = bdef_load-entities[ name = association-source_entity ]
                                                entity            = entity
                                                instance_to_check = <fs_retrieval_instance>
                                                operation         = if_abap_behv=>op-r-read_ba
                                              changing
                                                buffer_4_entity   = buffer_entry_for_assoc_entity
                                                c_reported        = reported
                                            ).

                data(result_configured) = find_result_config_for_rba(
                                              exporting
                                                parent_entity     = bdef_load-entities[ name = association-source_entity ]
                                                entity            = entity
                                                instance_to_check = <fs_retrieval_instance>
                                                operation         = if_abap_behv=>op-r-read_ba
                                              changing
                                                buffer_4_entity   = buffer_entry_for_assoc_entity
                                                c_result          = <fs_result>
                                            ).

                if reported_configured eq abap_false and failed_configured eq abap_false and result_configured eq abap_false.
                  " create a copy of buffer instance entry
                  deep_clone( exporting data_to_be_cloned = <assoc_entity_buffer_instance> importing reference_to_cloned_data = entity_buffer_instance_copy ).
                  assign entity_buffer_instance_copy->* to <entity_buffer_instance_copy>.

                  " format data as per the control structures provided
                  assign component '%control' of structure <fs_retrieval_instance> to <fs_control_fields>.
                  format_res_as_per_cntrl_flds(
                    exporting
                      control_fields  = <fs_control_fields>
                      entity_fields   = assoc_entity_fields
                    changing
                      entity_instance = <entity_buffer_instance_copy>
                        ).

                  append <entity_buffer_instance_copy> to <fs_result>.


                endif.
              endif.
            endloop.
          endif.
          continue.
        endif.
      endloop.

      " If the parent entity itself does not exist
      if parent_equal eq abap_false.
        " fill failed and reported

        alias = buffer_entry_for_entity-entity_alias.
        alias = to_upper( alias ).

        fill_read_reported_and_failed(
         exporting
          i_entity_fields = entity_fields
          alias_name = alias
          message_number = 001
          retrieval = retrieval
        changing
          c_failed = failed
          c_reported = reported
          cs_retrieval_instance = <fs_retrieval_instance> ).
      endif.

    endloop.

  endmethod.

  method zif_abap_behv_test_trans_bufr5~config_response_4_modify.

    data bdef_load type t_bdef_load.
    field-symbols: <buffer_4_entity>         type ty_double_transactional_buffer,
                   <entity_buffer_instances> type standard table,
                   <entity_buffer_instance>  type any,
                   <entity_reported>         type standard table.

    cl_abap_behv_load=>get_load(
                     exporting
                       entity   = root_name
                       all      = abap_true
                     importing
                       head         = bdef_load-head
                       entities     = bdef_load-entities
                       features     = bdef_load-features
                       actions      = bdef_load-actions
                       relations    = bdef_load-relations
                       associations = bdef_load-associations ).

    loop at bdef_load-entities into data(entity).
      if mapped is not initial.
        configure_mapped( entity = entity mapped = mapped operation = operation ).
      endif.

      if reported is not initial.
        configure_reported( entity = entity reported = reported operation = operation ).
      endif.

      if failed is not initial.
        configure_failed_for_modify( entity = entity failed = failed operation = operation ).
      endif.

    endloop.

  endmethod.

  method zif_abap_behv_test_trans_bufr5~config_response_4_read.

    data bdef_load type t_bdef_load.
    field-symbols: <buffer_4_entity>         type ty_double_transactional_buffer,
                   <entity_buffer_instances> type standard table,
                   <entity_buffer_instance>  type any,
                   <entity_reported>         type standard table.

    cl_abap_behv_load=>get_load(
                     exporting
                       entity   = root_name
                       all      = abap_true
                     importing
                       head         = bdef_load-head
                       entities     = bdef_load-entities
                       features     = bdef_load-features
                       actions      = bdef_load-actions
                       relations    = bdef_load-relations
                       associations = bdef_load-associations ).

    loop at bdef_load-entities into data(entity).
      if result is not initial.
        configure_result_for_read( entity = entity result = result operation = operation ).
      endif.

      if reported is not initial.
        configure_reported_for_read( entity = entity reported = reported operation = operation ).
      endif.

      if failed is not initial.
        configure_failed_for_read( entity = entity failed = failed operation = operation ).
      endif.

    endloop.

  endmethod.

  method get_entity_alias.
    if entity-alias is initial.
      alias = to_upper( entity-name ).
    else.
      alias = to_upper( entity-alias ).
    endif.
  endmethod.

  method configure_mapped.
    field-symbols: <buffer_4_entity>  type ty_double_transactional_buffer.
    field-symbols: <entity_mapped>  type standard table.
    field-symbols: <entity_reported_instances> type standard table.
    field-symbols: <mapped_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.


    data(alias) = get_entity_alias( entity = entity ).
    assign component alias of structure mapped to <entity_mapped>.

    if <entity_mapped> is assigned and <entity_mapped> is not initial.
      loop at <entity_mapped> assigning field-symbol(<entity_mapped_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_mapped_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-modify_response_config with key operation = operation assigning field-symbol(<response_config>).
              if  <response_config> is not assigned .
                "create an entry for the modify response config
                data mapped_config_for_op type ty_modify_response_config.
                create data mapped_config_for_op-mapped like mapped.
                mapped_config_for_op-operation = operation.

                assign mapped_config_for_op-mapped->* to field-symbol(<mapped_to_be_filled>).
                assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                append <entity_mapped_instance> to <mapped_entity_to_be_filled>.

                append mapped_config_for_op to <buffer_4_entity>-modify_response_config.

              else.

                "check if failed is configured for the operation and instance
                if <response_config>-failed is not initial.
                  data dummy_failed like <response_config>-failed.
                  data(failed_already_configured) = find_failed_if_configured(
                                                 exporting
                                                   entity            = entity
                                                   instance_to_check = <entity_mapped_instance>
                                                   operation         = operation
                                                 changing
                                                   buffer_4_entity   = <buffer_4_entity>
                                                   c_failed          = dummy_failed
                                               ).
                  if failed_already_configured eq abap_true.
                      "TODO: Exception reported is already configured. Configuring mapped for the same instance is undefined.
                    EXIT.
                  endif.
                endif.

                "check if reported is configured for the operation and instance
                if <response_config>-reported is not initial.
                  assign <response_config>-reported->* to field-symbol(<entity_reported>).
                  assign component alias of structure <entity_reported> to <entity_reported_instances>.
                  loop at <entity_reported_instances> assigning field-symbol(<entity_reported_instance>).
                    data(reported_already_configured) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_reported_instance>
                                   instance_2    = <entity_mapped_instance>
                                   entity_fields = entity_fields
                             ).
                    if reported_already_configured eq abap_true.
                      "TODO: Exception reported is already configured. Configuring mapped for the same instance is undefined.
                      EXIT.
                    endif.
                  endloop.
                  if reported_already_configured eq abap_true.
                    "TODO: Exception reported is already configured. Configuring mapped for the same instance is undefined.
                    EXIT.
                  endif.

                  "At this point it is clear that reported is not configured for the instance.
                  if <response_config>-mapped is initial.
                    create data <response_config>-mapped like mapped.
                    assign <response_config>-mapped->* to <mapped_to_be_filled>.
                    assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                    append <entity_mapped_instance> to <mapped_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-mapped->* to <mapped_to_be_filled>.
                    assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                    append <entity_mapped_instance> to <mapped_entity_to_be_filled>.

                  endif.

                else.
                  "At this point it is clear that reported is not configured for the instance.
                  if <response_config>-mapped is initial.
                    create data <response_config>-mapped like mapped.
                    assign <response_config>-mapped->* to <mapped_to_be_filled>.
                    assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                    append <entity_mapped_instance> to <mapped_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-mapped->* to <mapped_to_be_filled>.
                    assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                    append <entity_mapped_instance> to <mapped_entity_to_be_filled>.

                  endif.

                endif.
              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
            endif.
          endloop.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
        endif.
      endloop.
    endif.

  endmethod.

  method configure_reported.
    field-symbols: <buffer_4_entity>  type ty_double_transactional_buffer.
    field-symbols: <entity_mapped_instances>  type standard table.
    field-symbols: <entity_reported> type standard table.
    field-symbols: <reported_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.


    data(alias) = get_entity_alias( entity = entity ).
    assign component alias of structure reported to <entity_reported>.

    if <entity_reported> is assigned and <entity_reported> is not initial.
      loop at <entity_reported> assigning field-symbol(<entity_reported_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_reported_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-modify_response_config with key operation = operation assigning field-symbol(<response_config>).
              if  <response_config> is not assigned .
                "create an entry for the modify response config
                data reported_config_for_op type ty_modify_response_config.
                create data reported_config_for_op-reported like reported.
                reported_config_for_op-operation = operation.

                assign reported_config_for_op-reported->* to field-symbol(<reported_to_be_filled>).
                assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                append <entity_reported_instance> to <reported_entity_to_be_filled>.

                append reported_config_for_op to <buffer_4_entity>-modify_response_config.

              else.
                "check if mapped is configured for the operation and instance
                if <response_config>-mapped is not initial.
                  assign <response_config>-mapped->* to field-symbol(<entity_mapped>).
                  assign component alias of structure <entity_mapped> to <entity_mapped_instances>.
                  loop at <entity_mapped_instances> assigning field-symbol(<entity_mapped_instance>).
                    data(mapped_already_configured) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_mapped_instance>
                                   instance_2    = <entity_reported_instance>
                                   entity_fields = entity_fields
                             ).
                    if mapped_already_configured eq abap_true.
                      "TODO: Exception mapped is already configured. Configuring reported for the same instance is undefined.
                      EXIT.
                    endif.
                  endloop.
                  if mapped_already_configured eq abap_true.
                      "TODO: Exception mapped is already configured. Configuring reported for the same instance is undefined.
                    EXIT.
                  endif.

                  "At this point it is clear that mapped is not configured for the instance.
                  if <response_config>-reported is initial.
                    create data <response_config>-reported like reported.
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.

                  endif.

                else.
                  "At this point it is clear that mapped is not configured for the instance.
                  if <response_config>-reported is initial.
                    create data <response_config>-reported like reported.
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.

                  endif.

                endif.
              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
            endif.
          endloop.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
        endif.
      endloop.
    endif.

  endmethod.

  method configure_failed_for_modify.
    field-symbols: <buffer_4_entity>  type ty_double_transactional_buffer.
    field-symbols: <entity_mapped_instances>  type standard table.
    field-symbols: <entity_failed> type standard table.
    field-symbols: <failed_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.


    data(alias) = get_entity_alias( entity = entity ).
    assign component alias of structure failed to <entity_failed>.

    if <entity_failed> is assigned and <entity_failed> is not initial.
      loop at <entity_failed> assigning field-symbol(<entity_failed_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_failed_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-modify_response_config with key operation = operation assigning field-symbol(<response_config>).
              if <response_config> is not assigned.
                "create an entry for the modify response config
                data failed_config_for_op type ty_modify_response_config.
                create data failed_config_for_op-failed like failed.
                failed_config_for_op-operation = operation.

                assign failed_config_for_op-failed->* to field-symbol(<failed_to_be_filled>).
                assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                append <entity_failed_instance> to <failed_entity_to_be_filled>.

                append failed_config_for_op to <buffer_4_entity>-modify_response_config.

              else.
                "check if mapped is configured for the operation and instance
                if <response_config>-mapped is not initial.
                  assign <response_config>-mapped->* to field-symbol(<entity_mapped>).
                  assign component alias of structure <entity_mapped> to <entity_mapped_instances>.
                  loop at <entity_mapped_instances> assigning field-symbol(<entity_mapped_instance>).
                    data(mapped_already_configured) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_mapped_instance>
                                   instance_2    = <entity_failed_instance>
                                   entity_fields = entity_fields
                             ).
                    if mapped_already_configured eq abap_true.
                      "TODO: Exception mapped is already configured. Configuring failed for the same instance is undefined.
                      EXIT.
                    endif.
                  endloop.
                  if mapped_already_configured eq abap_true.
                      "TODO: Exception mapped is already configured. Configuring failed for the same instance is undefined.
                    EXIT.
                  endif.

                  "At this point it is clear that mapped is not configured for the instance.
                  if <response_config>-failed is initial.
                    create data <response_config>-failed like failed.
                    assign <response_config>-failed->* to <failed_to_be_filled>.
                    assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                    append <entity_failed_instance> to <failed_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-failed->* to <failed_to_be_filled>.
                    assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                    append <entity_failed_instance> to <failed_entity_to_be_filled>.

                  endif.

                else.
                  "At this point it is clear that mapped is not configured for the instance.
                  if <response_config>-failed is initial.
                    create data <response_config>-failed like failed.
                    assign <response_config>-failed->* to <failed_to_be_filled>.
                    assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                    append <entity_failed_instance> to <failed_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-failed->* to <failed_to_be_filled>.
                    assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                    append <entity_failed_instance> to <failed_entity_to_be_filled>.

                  endif.
                endif.
              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
            endif.
          endloop.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
        endif.
      endloop.
    endif.

  endmethod.

  method configure_result_for_read.
    field-symbols: <buffer_4_entity>  type ty_double_transactional_buffer.
    field-symbols: <entity_result> type standard table.
    field-symbols: <result_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.
    field-symbols: <reported_for_entity> type standard table.


    data(alias) = get_entity_alias( entity = entity ).

    if result is not initial.
      loop at result assigning field-symbol(<entity_result_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_result_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-read_response_config with key operation = operation assigning field-symbol(<response_config>).
              if <response_config> is not assigned .
                "create an entry for the read response config
                data result_config_for_op type ty_read_response_config.
                create data result_config_for_op-result like result.
                result_config_for_op-operation = operation.

                assign result_config_for_op-result->* to <result_entity_to_be_filled>.
                append <entity_result_instance> to <result_entity_to_be_filled>.

                append result_config_for_op to <buffer_4_entity>-read_response_config.

              else.

                "check if failed is configured for the operation and instance
                if <response_config>-failed is not initial.
                  data dummy_failed like <response_config>-failed.
                  data(failed_already_configured) = find_failed_config_for_read(
                                                 exporting
                                                   entity            = entity
                                                   instance_to_check = <entity_result_instance>
                                                   operation         = operation
                                                 changing
                                                   buffer_4_entity   = <buffer_4_entity>
                                                   c_failed          = dummy_failed
                                               ).
                  if failed_already_configured eq abap_true.
                      "TODO: Exception failed is already configured. Configuring result for the same instance is undefined.
                    EXIT.
                  endif.
                endif.

                "check if reported is configured for the operation and instance
                if <response_config>-reported is not initial.
                  data dummy_reported like <response_config>-reported.
                  data(reported_already_configured) = find_reported_config_for_read(
                                                 exporting
                                                   entity            = entity
                                                   instance_to_check = <entity_result_instance>
                                                   operation         = operation
                                                 changing
                                                   buffer_4_entity   = <buffer_4_entity>
                                                   c_reported          = dummy_reported
                                               ).
                  if reported_already_configured eq abap_true.
                      "TODO: Exception reported is already configured. Configuring result for the same instance is undefined.
                    EXIT.
                  endif.
                endif.

                "At this point it is clear that reported or failed is not configured.

                if <response_config>-result is not initial.
                  assign <response_config>-result->* to <result_entity_to_be_filled>.
                  append <entity_result_instance> to <result_entity_to_be_filled>.
                else.
                  create data <response_config>-result like result.
                  <response_config>-operation = operation.
                  assign <response_config>-result->* to <result_entity_to_be_filled>.
                  append <entity_result_instance> to <result_entity_to_be_filled>.
                endif.

              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
            endif.
          endloop.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
        endif.
      endloop.
    endif.

  endmethod.

  method configure_reported_for_read.
    field-symbols: <buffer_4_entity>  type ty_double_transactional_buffer.
    field-symbols: <entity_reported> type standard table.
    field-symbols: <reported_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.
    field-symbols: <result_for_entity> type standard table.


    data(alias) = get_entity_alias( entity = entity ).
    assign component alias of structure reported to <entity_reported>.

    if <entity_reported> is assigned and <entity_reported> is not initial.
      loop at <entity_reported> assigning field-symbol(<entity_reported_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_reported_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-read_response_config with key operation = operation assigning field-symbol(<response_config>).
              if  <response_config> is not assigned.
                "create an entry for the read response config
                data reported_config_for_op type ty_read_response_config.
                create data reported_config_for_op-reported like reported.
                reported_config_for_op-operation = operation.

                assign reported_config_for_op-reported->* to field-symbol(<reported_to_be_filled>).
                assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                append <entity_reported_instance> to <reported_entity_to_be_filled>.

                append reported_config_for_op to <buffer_4_entity>-read_response_config.

              else.
                "check if result is configured for the operation and instance
                if <response_config>-result is not initial.
                  assign <response_config>-result->* to <result_for_entity>.
                  loop at <result_for_entity> assigning field-symbol(<entity_result_instance>).
                    data(result_already_configured) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_result_instance>
                                   instance_2    = <entity_reported_instance>
                                   entity_fields = entity_fields
                             ).
                    if result_already_configured eq abap_true.
                      "TODO: Exception result is already configured. Configuring reported for the same instance is undefined.
                      EXIT.
                    endif.
                  endloop.
                  if result_already_configured eq abap_true.
                      "TODO: Exception result is already configured. Configuring reported for the same instance is undefined.
                    EXIT.
                  endif.

                  "At this point it is clear that result is not configured for the instance.
                  if <response_config>-reported is initial.
                    create data <response_config>-reported like reported.
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.

                  endif.

                else.
                  "At this point it is clear that result is not configured for the instance.
                  if <response_config>-reported is initial.
                    create data <response_config>-reported like reported.
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.

                  endif.

                endif.
              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
            endif.
          endloop.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
        endif.
      endloop.
    endif.

  endmethod.

  method configure_failed_for_read.
    field-symbols: <buffer_4_entity>  type ty_double_transactional_buffer.
    field-symbols: <entity_failed> type standard table.
    field-symbols: <failed_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.
    field-symbols: <result_for_entity> type standard table.


    data(alias) = get_entity_alias( entity = entity ).
    assign component alias of structure failed to <entity_failed>.

    if <entity_failed> is assigned and <entity_failed> is not initial.
      loop at <entity_failed> assigning field-symbol(<entity_failed_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_failed_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-read_response_config with key operation = operation assigning field-symbol(<response_config>).
              if <response_config> is not assigned.
                "create an entry for the read response config
                data failed_config_for_op type ty_read_response_config.
                create data failed_config_for_op-failed like failed.
                failed_config_for_op-operation = operation.

                assign failed_config_for_op-failed->* to field-symbol(<failed_to_be_filled>).
                assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                append <entity_failed_instance> to <failed_entity_to_be_filled>.

                append failed_config_for_op to <buffer_4_entity>-read_response_config.

              else.
                "check if result is configured for the operation and instance
                if <response_config>-result is not initial.
                  assign <response_config>-result->* to <result_for_entity>.
                  loop at <result_for_entity> assigning field-symbol(<entity_result_instance>).
                    data(result_already_configured) = check_instance_equal(
                                 exporting
                                   instance_1    = <entity_result_instance>
                                   instance_2    = <entity_failed_instance>
                                   entity_fields = entity_fields
                             ).
                    if result_already_configured eq abap_true.
                      "TODO: Exception result is already configured. Configuring failed for the same instance is undefined.
                      EXIT.
                    endif.
                  endloop.
                  if result_already_configured eq abap_true.
                      "TODO: Exception result is already configured. Configuring failed for the same instance is undefined.
                    EXIT.
                  endif.

                  "At this point it is clear that result is not configured for the instance.
                  if <response_config>-failed is initial.
                    create data <response_config>-failed like failed.
                    assign <response_config>-failed->* to <failed_to_be_filled>.
                    assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                    append <entity_failed_instance> to <failed_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-failed->* to <failed_to_be_filled>.
                    assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                    append <entity_failed_instance> to <failed_entity_to_be_filled>.

                  endif.

                else.
                  "At this point it is clear that result is not configured for the instance.
                  if <response_config>-failed is initial.
                    create data <response_config>-failed like failed.
                    assign <response_config>-failed->* to <failed_to_be_filled>.
                    assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                    append <entity_failed_instance> to <failed_entity_to_be_filled>.

                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-failed->* to <failed_to_be_filled>.
                    assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                    append <entity_failed_instance> to <failed_entity_to_be_filled>.

                  endif.

                endif.
              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
            endif.
          endloop.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
        endif.
      endloop.
    endif.

  endmethod.

  method find_reported_if_configured.

    field-symbols: <configured_entity_reported> type standard table.
    field-symbols: <existing_buff_reported> type any.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-modify_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-reported is not initial.
      assign response_config-reported->* to field-symbol(<configured_reported>).
      assign component alias of structure <configured_reported> to <configured_entity_reported>.
    endif.

    if <configured_entity_reported> is assigned and <configured_entity_reported> is not initial.
      loop at <configured_entity_reported> assigning field-symbol(<configured_reported_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_reported_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.
          field-symbols <fs_entry_reported_table> type standard table.
          assign component alias of structure c_reported to <fs_entry_reported_table>.
          append <configured_reported_instance> to <fs_entry_reported_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.

  method find_failed_if_configured.

    field-symbols: <configured_entity_failed> type standard table.
    field-symbols: <existing_buff_failed> type any.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-modify_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-failed is not initial.
      assign response_config-failed->* to field-symbol(<configured_failed>).
      assign component alias of structure <configured_failed> to <configured_entity_failed>.
    endif.

    if <configured_entity_failed> is assigned and <configured_entity_failed> is not initial.
      loop at <configured_entity_failed> assigning field-symbol(<configured_failed_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_failed_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.
          field-symbols <fs_entry_failed_table> type standard table.
          assign component alias of structure c_failed to <fs_entry_failed_table>.
          append <configured_failed_instance> to <fs_entry_failed_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.

  method find_mapped_if_configured.
    field-symbols: <configured_entity_mapped> type standard table.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-modify_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-mapped is not initial.
      assign response_config-mapped->* to field-symbol(<configured_mapped>).
      assign component alias of structure <configured_mapped> to <configured_entity_mapped>.
    endif.

    if <configured_entity_mapped> is assigned and  <configured_entity_mapped> is not initial.
      loop at <configured_entity_mapped> assigning field-symbol(<configured_mapped_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_mapped_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.
          field-symbols <fs_entry_mapped_table> type standard table.
          assign component alias of structure c_mapped to <fs_entry_mapped_table>.
          append <configured_mapped_instance> to <fs_entry_mapped_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.
  endmethod.

  method find_reported_config_for_read.

    field-symbols: <configured_entity_reported> type standard table.
    field-symbols: <existing_buff_reported> type any.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-read_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-reported is not initial.
      assign response_config-reported->* to field-symbol(<configured_reported>).
      assign component alias of structure <configured_reported> to <configured_entity_reported>.
    endif.

    if <configured_entity_reported> is assigned and <configured_entity_reported> is not initial.
      loop at <configured_entity_reported> assigning field-symbol(<configured_reported_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_reported_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.
          field-symbols <fs_entry_reported_table> type standard table.
          assign component alias of structure c_reported to <fs_entry_reported_table>.
          append <configured_reported_instance> to <fs_entry_reported_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.

  method find_reported_config_for_rba.

    field-symbols: <configured_entity_reported> type standard table.
    field-symbols: <existing_buff_reported> type any.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-read_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-reported is not initial.
      assign response_config-reported->* to field-symbol(<configured_reported>).
      assign component alias of structure <configured_reported> to <configured_entity_reported>.
    endif.

    if <configured_entity_reported> is assigned and <configured_entity_reported> is not initial.
      loop at <configured_entity_reported> assigning field-symbol(<configured_reported_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_reported_instance>
                         entity_fields = get_entity_fields( parent_entity-name )
                   ).

        if equal = abap_true.
          field-symbols <fs_entry_reported_table> type standard table.
          assign component alias of structure c_reported to <fs_entry_reported_table>.
          append <configured_reported_instance> to <fs_entry_reported_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.


  method find_failed_config_for_read.

    field-symbols: <configured_entity_failed> type standard table.
    field-symbols: <existing_buff_failed> type any.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-read_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-failed is not initial.
      assign response_config-failed->* to field-symbol(<configured_failed>).
      assign component alias of structure <configured_failed> to <configured_entity_failed>.
    endif.

    if <configured_entity_failed> is assigned and <configured_entity_failed> is not initial.
      loop at <configured_entity_failed> assigning field-symbol(<configured_failed_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_failed_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.
          field-symbols <fs_entry_failed_table> type standard table.
          assign component alias of structure c_failed to <fs_entry_failed_table>.
          append <configured_failed_instance> to <fs_entry_failed_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.

  method find_failed_config_for_rba.

    field-symbols: <configured_entity_failed> type standard table.
    field-symbols: <existing_buff_failed> type any.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-read_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-failed is not initial.
      assign response_config-failed->* to field-symbol(<configured_failed>).
      assign component alias of structure <configured_failed> to <configured_entity_failed>.
    endif.

    if <configured_entity_failed> is assigned and <configured_entity_failed> is not initial.
      loop at <configured_entity_failed> assigning field-symbol(<configured_failed_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_failed_instance>
                         entity_fields = get_entity_fields( parent_entity-name )
                   ).

        if equal = abap_true.
          field-symbols <fs_entry_failed_table> type standard table.
          assign component alias of structure c_failed to <fs_entry_failed_table>.
          append <configured_failed_instance> to <fs_entry_failed_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.


  method find_result_config_for_read.
    field-symbols: <configured_entity_result> type standard table.
    field-symbols: <existing_buff_result> type any.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-read_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-result is not initial.
      assign response_config-result->* to <configured_entity_result>.
    endif.

    if <configured_entity_result> is assigned and <configured_entity_result> is not initial.
      loop at <configured_entity_result> assigning field-symbol(<configured_result_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_result_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.

          " format data as per the control structures provided
          assign component '%control' of structure instance_to_check to field-symbol(<fs_control_fields>).
          format_res_as_per_cntrl_flds(
            exporting
              control_fields  = <fs_control_fields>
              entity_fields   = entity_fields
            changing
              entity_instance = <configured_result_instance>
          ).

          append <configured_result_instance> to c_result.

*          field-symbols <fs_entry_result_table> type standard table.
*          assign c_result to <fs_entry_result_table>.
*          append <configured_entity_result> to <fs_entry_result_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.

 method find_result_config_for_rba.
    field-symbols: <configured_entity_result> type standard table.
    field-symbols: <existing_buff_result> type any.

    data(alias) = get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-read_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-result is not initial.
      assign response_config-result->* to <configured_entity_result>.
    endif.

    if <configured_entity_result> is assigned and <configured_entity_result> is not initial.
      loop at <configured_entity_result> assigning field-symbol(<configured_result_instance>).
        data(equal) = check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_result_instance>
                         entity_fields = get_entity_fields( parent_entity-name )
                   ).

        if equal = abap_true.

          " format data as per the control structures provided
          assign component '%control' of structure instance_to_check to field-symbol(<fs_control_fields>).
          format_res_as_per_cntrl_flds(
            exporting
              control_fields  = <fs_control_fields>
              entity_fields   = entity_fields
            changing
              entity_instance = <configured_result_instance>
          ).

          append <configured_result_instance> to c_result.

*          field-symbols <fs_entry_result_table> type standard table.
*          assign c_result to <fs_entry_result_table>.
*          append <configured_entity_result> to <fs_entry_result_table>.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.


  method zif_abap_behv_test_trans_bufr5~clear_buffer.
    clear dbl_transactional_buffer_table.
  endmethod.

  method configure_failed.
  " refactoring attempt for configure failed for modify and read.
  endmethod.

endclass.
