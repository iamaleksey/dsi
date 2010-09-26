#include <erl_driver.h>

typedef struct {
    ErlDrvPort port;
} DsiData;

static int
dsi_init()
{
    return 0;
}

static ErlDrvData
dsi_start(const ErlDrvPort port, char *const command)
{
    DsiData *const dd = (DsiData*) driver_alloc(sizeof(DsiData));

    if (dd == NULL)
        return ERL_DRV_ERROR_GENERAL;

    dd->port = port;

    return (ErlDrvData) dd;
}

static void
dsi_stop(const ErlDrvData drv_data)
{
    driver_free((char*) drv_data);
}

static void
dsi_output(const ErlDrvData drv_data, char *const buf, const int len)
{
}

static ErlDrvEntry dsi_driver_entry =
{
    .init            = dsi_init,
    .start           = dsi_start,
    .stop            = dsi_stop,
    .driver_name     = "dsi_drv",
    .output          = dsi_output,
    .extended_marker = ERL_DRV_EXTENDED_MARKER,
    .major_version   = ERL_DRV_EXTENDED_MAJOR_VERSION,
    .minor_version   = ERL_DRV_EXTENDED_MINOR_VERSION,
    .driver_flags    = ERL_DRV_FLAG_USE_PORT_LOCKING
};

DRIVER_INIT (dsi_drv)
{
    return &dsi_driver_entry;
}
