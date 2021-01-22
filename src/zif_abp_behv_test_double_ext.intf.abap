interface zif_abp_behv_test_double_ext
  public .

  methods: execute_action
    importing
      !changes  type abp_behv_changes_tab
    changing
      !failed   type data
      !mapped   type data
      !reported type data .

endinterface.
