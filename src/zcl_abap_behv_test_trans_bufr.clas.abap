class zcl_abap_behv_test_trans_bufr definition public final create private.

  public section.
    interfaces zif_abap_behv_test_trans_bufr.

    class-methods:
      "! <p class="shorttext synchronized" lang="en">Get the transactional buffer instance</p>
      get_instance returning value(double_transactional_buffer) type ref to zif_abap_behv_test_trans_bufr.

  protected section.
  private section.

    types: begin of ty_double_transactional_buffer,
             entity_root      type abp_root_entity_name,
             entity_name      type abp_entity_name,
             entity_instances type ref to data,
           end of ty_double_transactional_buffer.

    class-data:
     "! <p class="shorttext synchronized" lang="en">Transactional buffer double singleton instance</p>
     double_transactional_buffer type ref to zif_abap_behv_test_trans_bufr.

    data:
     "! <p class="shorttext synchronized" lang="en">Transactional buffer double</p>
      dbl_transactional_buffer_table type standard table of ty_double_transactional_buffer with key entity_name.    " TODO : should we have a sorted table on entity name for binary search, or provide hashing to improve the search

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
            entity_name type abp_entity_name
        returning value(entity_fields) type ddfields,

      deep_clone
        importing
            data_to_be_cloned type any
        exporting
            reference_to_cloned_data type ref to data,

      format_res_as_per_cntrl_flds
        importing
            control_fields type any
            entity_fields type ddfields
        changing
            entity_instance type any.

endclass.


class zcl_abap_behv_test_trans_bufr implementation.

  method get_instance.
    if zcl_abap_behv_test_trans_bufr=>double_transactional_buffer is initial.
      zcl_abap_behv_test_trans_bufr=>double_transactional_buffer = new zcl_abap_behv_test_trans_bufr( ).
    endif.
    double_transactional_buffer = zcl_abap_behv_test_trans_bufr=>double_transactional_buffer.
  endmethod.

  method zif_abap_behv_test_trans_bufr~read.
    data entity_fields type ddfields.
    data buffer_entry_for_entity type ty_double_transactional_buffer.
    data entity_buffer_instance_copy type ref to data.

    field-symbols <fs_retrieval> type abp_behv_retrievals.
    field-symbols <fs_retrieval_instances> type standard table.
    field-symbols <fs_retrieval_instance> type any.
    field-symbols <fs_control_fields> type any.

    field-symbols <entity_buffer_instances> type standard table.
    field-symbols <entity_buffer_instance> type any.
    field-symbols <entity_buffer_instance_copy> type any.

    field-symbols <fs_result> type standard table.

    loop at retrievals assigning <fs_retrieval>.
      clear buffer_entry_for_entity.

      " retrieve the buffer entry for the entity
      if line_exists( dbl_transactional_buffer_table[ entity_name = <fs_retrieval>-entity_name ] ).
        buffer_entry_for_entity = dbl_transactional_buffer_table[ entity_name = <fs_retrieval>-entity_name ].
      endif.

      " TODO: Fill failed structure when the entry does not exist in the transactional buffer
      if buffer_entry_for_entity is initial.
        continue.
      endif.

      " get the reference to the entity instances in buffer
      assign buffer_entry_for_entity-entity_instances->* to <entity_buffer_instances>.
      " get the reference to the entity instances requested
      assign <fs_retrieval>-instances->* to <fs_retrieval_instances>.
      " get the reference to the output result
      assign <fs_retrieval>-results->* to <fs_result>.

      " retrieve fields of the entity
      entity_fields = get_entity_fields( <fs_retrieval>-entity_name ).

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

        endloop.

      endloop.

    endloop.
  endmethod.

  method zif_abap_behv_test_trans_bufr~modify.

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

    data buffer_entry_for_entity type ty_double_transactional_buffer.
    data struct_descr type ref to cl_abap_structdescr.
    data entity_fields type ddfields.
    data entity_field type dfies.
    data instance_found type abap_bool.
    data entity_buffer_instance type ref to data.

    field-symbols <fs_change> type abp_behv_changes.
    field-symbols <entity_buffer_instances> type standard table.
    field-symbols <entity_buffer_instance> type any.
    field-symbols <entity_buffer_instance_field> type any.
    field-symbols <fs_entity_instances_field> type any.
    field-symbols <fs_entity_instances_provided> type standard table.
    field-symbols <fs_entity_instance_provided> type any.
    field-symbols <fs_entry_mapped_table> type standard table.
    field-symbols <fs_entry_failed_table> type standard table.
    field-symbols <fs_entry_reported_table> type standard table.

    cl_abap_behv_load=>get_load(
                         exporting
                           entity   = root_name
                           all      = abap_true
                         importing
                           head         = data(head)
                           entities     = data(entities)
                           features     = data(features)
                           actions      = data(actions)
                           relations    = data(relations)
                           associations = data(associations) ).

    loop at changes assigning <fs_change>.
      clear buffer_entry_for_entity.

      if line_exists( dbl_transactional_buffer_table[ entity_name = <fs_change>-entity_name ] ).
        buffer_entry_for_entity = dbl_transactional_buffer_table[ entity_name = <fs_change>-entity_name ].
      endif.

      struct_descr ?= cl_abap_typedescr=>describe_by_name( <fs_change>-entity_name ).
      entity_fields = struct_descr->get_ddic_field_list( ).

      if buffer_entry_for_entity is initial.
        " no buffer entry present for the entity
        buffer_entry_for_entity-entity_name = <fs_change>-entity_name.
        buffer_entry_for_entity-entity_instances = cl_abap_behvdescr=>create_data(
                                                        p_name     = <fs_change>-entity_name
                                                        p_op       = if_abap_behv=>op-r-read
                                                        p_kind     = if_abap_behv=>typekind-result

                                                   ).
        append buffer_entry_for_entity to dbl_transactional_buffer_table.
      endif.

      assign buffer_entry_for_entity-entity_instances->* to <entity_buffer_instances>.
      assign <fs_change>-instances->* to <fs_entity_instances_provided>.

      if <fs_change>-op = if_abap_behv=>op-m-create.

        loop at <fs_entity_instances_provided> assigning <fs_entity_instance_provided>.

          "check whether the entity instance is already present in the buffer
          data(exists) = check_instance_exists(
                            exporting
                              instance_to_check = <fs_entity_instance_provided>
                              entity_instances  = <entity_buffer_instances>
                              entity_fields     = entity_fields
                         ).

          data(entity_details) = entities[ name = <fs_change>-entity_name ].
          data(alias) = entity_details-alias.
          alias = to_upper( alias ).

          if exists = abap_true.
            " fill failed and reported
            assign component alias of structure failed to <fs_entry_failed_table>.

            data entry_failed type ref to data.
            create data entry_failed like line of <fs_entry_failed_table>.
            assign entry_failed->* to field-symbol(<fs_entry_failed>).

            loop at entity_fields into entity_field where keyflag = abap_true.
              assign component entity_field-fieldname of structure <fs_entity_instance_provided> to field-symbol(<fs_val_1>).
              assign component entity_field-fieldname of structure <fs_entry_failed> to field-symbol(<fs_val_2>).
              <fs_val_2> = <fs_val_1>.
            endloop.
            append <fs_entry_failed> to <fs_entry_failed_table>.

            assign component alias of structure reported to <fs_entry_reported_table>.

            data entry_reported type ref to data.
            create data entry_reported like line of <fs_entry_reported_table>.
            assign entry_reported->* to field-symbol(<fs_entry_reported>).

            loop at entity_fields into entity_field where keyflag = abap_true.
              assign component entity_field-fieldname of structure <entity_buffer_instance> to <fs_val_1>.
              assign component entity_field-fieldname of structure <fs_entry_reported> to <fs_val_2>.
              <fs_val_2> = <fs_val_1>.
            endloop.
            append <fs_entry_reported> to <fs_entry_reported_table>.

          else.
            create data entity_buffer_instance like line of <entity_buffer_instances>.
            assign entity_buffer_instance->* to <entity_buffer_instance>.
            <entity_buffer_instance> = corresponding #( <fs_entity_instance_provided> ).
            append <entity_buffer_instance> to <entity_buffer_instances>.

            assign component alias of structure mapped to <fs_entry_mapped_table>.
            assign component alias of structure reported to <fs_entry_reported_table>.
            assign component alias of structure failed to <fs_entry_failed_table>.

            data entry_mapped type ref to data.
            create data entry_mapped like line of <fs_entry_mapped_table>.
            assign entry_mapped->* to field-symbol(<fs_entry_mapped>).

            loop at entity_fields into entity_field where keyflag = abap_true.
              assign component entity_field-fieldname of structure <entity_buffer_instance> to <fs_val_1>.
              assign component entity_field-fieldname of structure <fs_entry_mapped> to <fs_val_2>.
              <fs_val_2> = <fs_val_1>.
            endloop.
            append <fs_entry_mapped> to <fs_entry_mapped_table>.
          endif.
        endloop.

      endif.

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

endclass.
