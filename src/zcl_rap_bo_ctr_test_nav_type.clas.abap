class zcl_rap_bo_ctr_test_nav_type definition
  public
  final
  create public .

  public section.

    interfaces zif_rap_bo_ctr_tst_nav_typ_frw .
    methods constructor
    importing property_name type string.
  protected section.
  private section.
    data: property_name         type string,
          navigate_to_bdef      type abap_bool,
          navigate_to_behv_impl type abap_bool,
          naviagate_to_cds      type abap_bool.

endclass.



class zcl_rap_bo_ctr_test_nav_type implementation.

 method constructor.

    me->property_name = property_name.

 endmethod.


  method zif_rap_bo_ctr_tst_nav_typ_frw~is_navigation_to_cds.

    is_navigation_to_cds = me->naviagate_to_cds.

  endmethod.


  method zif_rap_bo_ctr_tst_nav_typ_frw~is_navigation_to_bdef.

    is_navigation_to_bdef = me->navigate_to_bdef.

  endmethod.


  method zif_rap_bo_ctr_tst_nav_typ_frw~is_navigation_to_behv_impl.

    is_navigation_to_behv_impl = me->navigate_to_behv_impl.

  endmethod.

  method zif_rap_bo_ctr_test_navig_type~navigate_to_bdef.

    me->navigate_to_bdef = abap_true.

  endmethod.

  method zif_rap_bo_ctr_test_navig_type~navigate_to_behavior_impl.

    me->navigate_to_behv_impl = abap_true.

  endmethod.

  method zif_rap_bo_ctr_test_navig_type~navigate_to_cds.

    me->naviagate_to_cds = abap_true.

  endmethod.

  method zif_rap_bo_ctr_tst_nav_typ_frw~get_property_name.

    property_name = me->property_name.

  endmethod.

endclass.
