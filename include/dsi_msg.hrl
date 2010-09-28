-record(dsi_hdr, {type         :: 0..65535,        % type of message.
                  id       = 0 :: 0..65535,        % module instatiation.
                  instance = 0 :: 0..65535,        % module instance.
                  src      = 0 :: 0..255,          % sending module ID.
                  dst          :: 0..255,          % destination module ID.
                  status   = 0 :: 0..255,          % returned status.
                  err_info = 0 :: 0..4294967295}). % status infromation.

-record(dsi_msg, {hdr :: #dsi_hdr{}, body :: binary()}).
