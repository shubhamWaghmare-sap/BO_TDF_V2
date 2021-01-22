class zcl_abap_behv_cid_mapper definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_cid_to_inst_map,
        cid   type abp_behv_cid,
        inst  type ref to data,
      end of   ty_cid_to_inst_map.

    types: tt_cid_to_inst_map type hashed table of ty_cid_to_inst_map with unique key cid.

    methods add
      importing cid       type abp_behv_cid
                instance  type ref to data .

    methods get
      importing cid              type abp_behv_cid
      returning value(instance)  type ref to data .


  protected section.
  private section.
    data cid_to_inst_map type tt_cid_to_inst_map.

endclass.



class zcl_abap_behv_cid_mapper implementation.
  method add.
    data line type ty_cid_to_inst_map.
    line-cid = cid.
    line-inst = instance.

    insert line into table cid_to_inst_map.

  endmethod.

  method get.
    read table cid_to_inst_map with key cid = cid into data(map_entry).
    instance = map_entry-inst.
  endmethod.

endclass.
