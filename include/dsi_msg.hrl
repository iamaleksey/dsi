-record(dsi_hdr, {type         :: non_neg_integer(),   % type of message.
                  id       = 0 :: non_neg_integer(),   % module instatiation.
                  instance = 0 :: non_neg_integer(),   % module instance.
                  src      = 0 :: non_neg_integer(),   % sending module ID.
                  dst          :: non_neg_integer(),   % destination module ID.
                  status   = 0 :: non_neg_integer(),   % returned status.
                  err_info = 0 :: non_neg_integer()}). % status infromation.

-record(dsi_msg, {hdr :: #dsi_hdr{}, body :: binary()}).
