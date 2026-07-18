#include <stdint.h>

#include <lean/lean.h>

extern uint32_t gimlight_set_raw_mode_raw(void);
extern uint32_t gimlight_restore_terminal_raw(void);
extern uint32_t gimlight_render_text_raw(const char *text);
extern uint32_t gimlight_read_input_raw(void);

LEAN_EXPORT lean_obj_res gimlight_set_raw_mode(void) {
    return lean_io_result_mk_ok(lean_box_uint32(gimlight_set_raw_mode_raw()));
}

LEAN_EXPORT lean_obj_res gimlight_restore_terminal(void) {
    return lean_io_result_mk_ok(
        lean_box_uint32(gimlight_restore_terminal_raw()));
}

LEAN_EXPORT lean_obj_res gimlight_render_text(b_lean_obj_arg text) {
    return lean_io_result_mk_ok(
        lean_box_uint32(gimlight_render_text_raw(lean_string_cstr(text))));
}

LEAN_EXPORT lean_obj_res gimlight_read_input(void) {
    return lean_io_result_mk_ok(lean_box_uint32(gimlight_read_input_raw()));
}
