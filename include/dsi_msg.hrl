-record(dsi_hdr, {type         :: non_neg_integer(),
                  id = 0       :: non_neg_integer(),
                  src = 0      :: non_neg_integer(),
                  dst          :: non_neg_integer(),
                  status = 0   :: non_neg_integer(),
                  err_info = 0 :: non_neg_integer()}).

-record(dsi_msg, {hdr  :: #'dsi_hdr'{},
                  body :: binary()}).
