#include <erl_driver.h>

#include <string.h>

#include "system.h"
#include "msg.h"
#include "sysgct.h"
#include "pack.h"
#include "confirm.h"
#include "ss7_inc.h"

#define DSI_SEND         0
#define DSI_RECV         1
#define DSI_GRAB         2
#define DSI_LINK         3
#define DSI_UNLINK       4
#define DSI_IS_CONGESTED 5

#define ATOM_SPEC_LEN    6
#define OK_MSG_SPEC_LEN  33

#define ERL_HDR_LEN 13

typedef struct {
    ErlDrvPort port;
    ErlDrvTermData* ok_msg_spec;
} DsiData;

ErlDrvTermData* ok_spec    = NULL;
ErlDrvTermData* error_spec = NULL;
ErlDrvTermData* empty_spec = NULL;
ErlDrvTermData* false_spec = NULL;
ErlDrvTermData* true_spec  = NULL;

static void
fill_ok_msg_spec(ErlDrvTermData* spec, MSG* msg)
{
    spec[9]  = msg->hdr.type;
    spec[11] = msg->hdr.id;
    spec[13] = GCT_get_instance(&msg->hdr);
    spec[15] = msg->hdr.src;
    spec[17] = msg->hdr.dst;
    spec[19] = msg->hdr.status;
    spec[21] = msg->hdr.err_info;
    spec[25] = (ErlDrvTermData) get_param(msg);
    spec[26] = msg->len;
}

static void
reset_ok_msg_spec(ErlDrvTermData* spec)
{
    spec[9]  = 0;
    spec[11] = 0;
    spec[13] = 0;
    spec[15] = 0;
    spec[17] = 0;
    spec[19] = 0;
    spec[21] = 0;
    spec[25] = (ErlDrvTermData) NULL;
    spec[26] = 0;
}

static void
dsi_send(DsiData* dd, char module_id, char* buf, int len)
{
    MSG* msg;
    u8*  pptr;

    u16 type = (u16) runpackbytes((u8*) buf, 0, 2);
    u16 id   = (u16) runpackbytes((u8*) buf, 2, 2);

    if ((msg = getm(type, id, 0, len - ERL_HDR_LEN)) != 0) {
        GCT_set_instance((u16) runpackbytes((u8*) buf, 4, 2), &msg->hdr);

        msg->hdr.src      = (u8)  runpackbytes((u8*) buf, 6, 1);
        msg->hdr.dst      = (u8)  runpackbytes((u8*) buf, 7, 1);
        msg->hdr.status   = (u8)  runpackbytes((u8*) buf, 8, 1);
        msg->hdr.err_info = (u32) runpackbytes((u8*) buf, 9, 4);

        pptr = get_param(msg);
        memset(pptr, 0, msg->len);
        memcpy(pptr, buf + ERL_HDR_LEN, msg->len);

        if (GCT_send(msg->hdr.dst, &msg->hdr) != 0) {
            relm(&msg->hdr);
            driver_output_term(dd->port, error_spec, ATOM_SPEC_LEN);
        } else {
            driver_output_term(dd->port, ok_spec, ATOM_SPEC_LEN);
        }
    } else {
        driver_output_term(dd->port, error_spec, ATOM_SPEC_LEN);
    }
}

static void
dsi_recv(DsiData* dd, unsigned char module_id)
{
    MSG* msg;

    if ((msg = (MSG*) GCT_receive(module_id)) != 0) {
        fill_ok_msg_spec(dd->ok_msg_spec, msg);
        driver_output_term(dd->port, dd->ok_msg_spec, OK_MSG_SPEC_LEN);
        reset_ok_msg_spec(dd->ok_msg_spec);
        confirm_msg(msg);
    } else {
        driver_output_term(dd->port, error_spec, ATOM_SPEC_LEN);
    }
}

static void
dsi_grab(DsiData* dd, unsigned char module_id)
{
    MSG* msg;

    if ((msg = (MSG*) GCT_grab(module_id)) != 0) {
        fill_ok_msg_spec(dd->ok_msg_spec, msg);
        driver_output_term(dd->port, dd->ok_msg_spec, OK_MSG_SPEC_LEN);
        reset_ok_msg_spec(dd->ok_msg_spec);
        confirm_msg(msg);
    } else {
        driver_output_term(dd->port, empty_spec, ATOM_SPEC_LEN);
    }
}

static void
dsi_link(DsiData* dd)
{
    if (GCT_link() == 0) {
        driver_output_term(dd->port, true_spec, ATOM_SPEC_LEN);
    } else {
        driver_output_term(dd->port, false_spec, ATOM_SPEC_LEN);
    }
}

static void
dsi_unlink(DsiData* dd)
{
    GCT_unlink();
    driver_output_term(dd->port, ok_spec, ATOM_SPEC_LEN);
}

static void
dsi_is_congested(DsiData* dd, unsigned char type)
{
    // type == 0 (standard) or 1 (long).
    if (GCT_partition_congestion(type) == NO_CONGESTION) {
        driver_output_term(dd->port, false_spec, ATOM_SPEC_LEN);
    } else {
        driver_output_term(dd->port, true_spec, ATOM_SPEC_LEN);
    }
}

static int
dsi_init()
{
    // {dsi_reply, ok}.
    ok_spec =
        (ErlDrvTermData*) driver_alloc(ATOM_SPEC_LEN * sizeof(ErlDrvTermData));

    if (ok_spec == NULL)
        return -1;

    ok_spec[0] = ERL_DRV_ATOM;
    ok_spec[1] = driver_mk_atom("dsi_reply");
    ok_spec[2] = ERL_DRV_ATOM;
    ok_spec[3] = driver_mk_atom("ok");
    ok_spec[4] = ERL_DRV_TUPLE;
    ok_spec[5] = 2;

    // {dsi_reply, error}.
    error_spec =
        (ErlDrvTermData*) driver_alloc(ATOM_SPEC_LEN * sizeof(ErlDrvTermData));

    if (error_spec == NULL)
        return -1;

    error_spec[0] = ERL_DRV_ATOM;
    error_spec[1] = driver_mk_atom("dsi_reply");
    error_spec[2] = ERL_DRV_ATOM;
    error_spec[3] = driver_mk_atom("error");
    error_spec[4] = ERL_DRV_TUPLE;
    error_spec[5] = 2;

    // {dsi_reply, empty}.
    empty_spec =
        (ErlDrvTermData*) driver_alloc(ATOM_SPEC_LEN * sizeof(ErlDrvTermData));

    if (empty_spec == NULL)
        return -1;

    empty_spec[0] = ERL_DRV_ATOM;
    empty_spec[1] = driver_mk_atom("dsi_reply");
    empty_spec[2] = ERL_DRV_ATOM;
    empty_spec[3] = driver_mk_atom("empty");
    empty_spec[4] = ERL_DRV_TUPLE;
    empty_spec[5] = 2;

    // {dsi_reply, false}.
    false_spec =
        (ErlDrvTermData*) driver_alloc(ATOM_SPEC_LEN * sizeof(ErlDrvTermData));

    if (false_spec == NULL)
        return -1;

    false_spec[0] = ERL_DRV_ATOM;
    false_spec[1] = driver_mk_atom("dsi_reply");
    false_spec[2] = ERL_DRV_ATOM;
    false_spec[3] = driver_mk_atom("false");
    false_spec[4] = ERL_DRV_TUPLE;
    false_spec[5] = 2;

    // {dsi_reply, true}.
    true_spec =
        (ErlDrvTermData*) driver_alloc(ATOM_SPEC_LEN * sizeof(ErlDrvTermData));

    if (true_spec == NULL)
        return -1;

    true_spec[0] = ERL_DRV_ATOM;
    true_spec[1] = driver_mk_atom("dsi_reply");
    true_spec[2] = ERL_DRV_ATOM;
    true_spec[3] = driver_mk_atom("true");
    true_spec[4] = ERL_DRV_TUPLE;
    true_spec[5] = 2;

    return 0;
}

static void
dsi_finish()
{
    driver_free((char*) ok_spec);
    driver_free((char*) error_spec);
    driver_free((char*) empty_spec);
    driver_free((char*) false_spec);
    driver_free((char*) true_spec);
}

static ErlDrvData
dsi_start(ErlDrvPort port, char* command)
{
    DsiData* dd = (DsiData*) driver_alloc(sizeof(DsiData));

    if (dd == NULL)
        return ERL_DRV_ERROR_GENERAL;

    dd->port = port;

    // {dsi_reply, {ok, {dsi_msg, {dsi_hdr, 0, 0, 0, 0, 0, 0, 0}, <<>>}}}.
    dd->ok_msg_spec =
        (ErlDrvTermData*) driver_alloc(OK_MSG_SPEC_LEN * sizeof(ErlDrvTermData));

    if (dd->ok_msg_spec == NULL)
        return ERL_DRV_ERROR_GENERAL;

    // {dsi_reply,
    dd->ok_msg_spec[0] = ERL_DRV_ATOM;
    dd->ok_msg_spec[1] = driver_mk_atom("dsi_reply");
    //     {ok,
    dd->ok_msg_spec[2] = ERL_DRV_ATOM;
    dd->ok_msg_spec[3] = driver_mk_atom("ok");
    //         {dsi_msg,
    dd->ok_msg_spec[4] = ERL_DRV_ATOM;
    dd->ok_msg_spec[5] = driver_mk_atom("dsi_msg");
    //             {dsi_hdr,
    dd->ok_msg_spec[6] = ERL_DRV_ATOM;
    dd->ok_msg_spec[7] = driver_mk_atom("dsi_hdr");
    //                 type,
    dd->ok_msg_spec[8] = ERL_DRV_UINT;
    dd->ok_msg_spec[9] = 0;
    //                 id,
    dd->ok_msg_spec[10] = ERL_DRV_UINT;
    dd->ok_msg_spec[11] = 0;
    //                 instance,
    dd->ok_msg_spec[12] = ERL_DRV_UINT;
    dd->ok_msg_spec[13] = 0;
    //                 src,
    dd->ok_msg_spec[14] = ERL_DRV_UINT;
    dd->ok_msg_spec[15] = 0;
    //                 dst,
    dd->ok_msg_spec[16] = ERL_DRV_UINT;
    dd->ok_msg_spec[17] = 0;
    //                 status,
    dd->ok_msg_spec[18] = ERL_DRV_UINT;
    dd->ok_msg_spec[19] = 0;
    //                 err_info
    dd->ok_msg_spec[20] = ERL_DRV_UINT;
    dd->ok_msg_spec[21] = 0;
    //             }, dsi_hdr
    dd->ok_msg_spec[22] = ERL_DRV_TUPLE;
    dd->ok_msg_spec[23] = 8;
    //             <<>>
    dd->ok_msg_spec[24] = ERL_DRV_BUF2BINARY;
    dd->ok_msg_spec[25] = (ErlDrvTermData) NULL;
    dd->ok_msg_spec[26] = 0;
    //         } dsi_msg
    dd->ok_msg_spec[27] = ERL_DRV_TUPLE;
    dd->ok_msg_spec[28] = 3;
    //     } ok
    dd->ok_msg_spec[29] = ERL_DRV_TUPLE;
    dd->ok_msg_spec[30] = 2;
    // } dsi_reply
    dd->ok_msg_spec[31] = ERL_DRV_TUPLE;
    dd->ok_msg_spec[32] = 2;

    return (ErlDrvData) dd;
}

static void
dsi_stop(ErlDrvData drv_data)
{
    DsiData* dd = (DsiData*) drv_data;

    driver_free((char*) dd->ok_msg_spec);
    driver_free((char*) drv_data);
}

static void
dsi_output(ErlDrvData drv_data, char* buf, int len)
{
    DsiData* dd = (DsiData*) drv_data;

    switch (*buf) {
        case DSI_SEND:
            dsi_send(dd, *(buf + 1), buf + 2, len - 2);
            break;

        case DSI_RECV:
            dsi_recv(dd, *(buf + 1));
            break;

        case DSI_GRAB:
            dsi_grab(dd, *(buf + 1));
            break;

        case DSI_LINK:
            dsi_link(dd);
            break;

        case DSI_UNLINK:
            dsi_unlink(dd);
            break;

        case DSI_IS_CONGESTED:
            dsi_is_congested(dd, *(buf + 1));
            break;
    }
}

static ErlDrvEntry dsi_driver_entry =
{
    .driver_name     = "dsi_drv",
    .init            = dsi_init,
    .finish          = dsi_finish,
    .start           = dsi_start,
    .stop            = dsi_stop,
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
