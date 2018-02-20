{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Nuklear where

import Foreign.Ptr
import qualified Language.C.Inline as C

C.verbatim "#define NK_IMPLEMENTATION"
C.verbatim "#define NK_INCLUDE_DEFAULT_ALLOCATOR"
C.verbatim "#define NK_INCLUDE_FONT_BAKING"
C.verbatim "#define NK_INCLUDE_DEFAULT_FONT"
C.verbatim "#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT"
C.include "../nuklear/nuklear.h"
C.include "<stdio.h>"

C.verbatim "typedef struct {"
C.verbatim "  struct nk_buffer cmds;"
C.verbatim "  struct nk_draw_null_texture null;"
C.verbatim "} nk_device;"

C.verbatim "typedef struct {"
C.verbatim "  float position[2];"
C.verbatim "  float uv[2];"
C.verbatim "  nk_byte col[4];"
C.verbatim "} nk_vertex;"

C.verbatim "typedef struct {"
C.verbatim "  struct nk_context ctx;"
C.verbatim "  nk_device ogl;"
C.verbatim "  struct nk_font_atlas atlas;"
C.verbatim "} nk_ctx;"

data NK = NK (Ptr ()) deriving Show

initNuklear :: IO NK
initNuklear = do
  ctx <- [C.block| void* {
    nk_ctx* ctx = calloc(1, sizeof(nk_ctx));

    nk_init_default(&ctx->ctx, 0);
    nk_buffer_init_default(&ctx->ogl.cmds);

    nk_font_atlas_init_default(&ctx->atlas);
    nk_font_atlas_begin(&ctx->atlas);

    void* image; int w, h;
    image = nk_font_atlas_bake(&ctx->atlas, &w, &h, NK_FONT_ATLAS_RGBA32);

    nk_font_atlas_end(&ctx->atlas, nk_handle_ptr(image), &ctx->ogl.null);

    if (ctx->atlas.default_font)
       nk_style_set_font(&ctx->ctx, &ctx->atlas.default_font->handle);

    return ctx;
  } |]
  pure $ NK ctx

destroyNuklear :: NK -> IO ()
destroyNuklear (NK ptr) = do
  [C.block| void {
    nk_ctx* ctx = $(void* ptr);
    free(ctx);
  } |]

test :: NK -> IO ()
test (NK ptr) = do
  [C.block| void {
    nk_ctx* NK = $(void* ptr);
    struct nk_context* ctx = &NK->ctx;

    nk_input_begin(ctx);
    nk_input_end(ctx);

    struct nk_colorf bg;
    bg.r = 0.1f; bg.g = 0.18f; bg.b = 0.24f; bg.a = 1.0f;

    if (nk_begin(ctx, "Demo", nk_rect(50, 50, 230, 250),
        NK_WINDOW_BORDER|NK_WINDOW_MOVABLE|NK_WINDOW_SCALABLE|NK_WINDOW_MINIMIZABLE|NK_WINDOW_TITLE))
    {
      enum {EASY, HARD};
      static int op = EASY;
      static int property = 20;

      nk_layout_row_static(ctx, 30, 80, 1);
      if (nk_button_label(ctx, "button"))
          fprintf(stdout, "button pressed\n");
      nk_layout_row_dynamic(ctx, 30, 2);
      if (nk_option_label(ctx, "easy", op == EASY)) op = EASY;
      if (nk_option_label(ctx, "hard", op == HARD)) op = HARD;
      nk_layout_row_dynamic(ctx, 25, 1);
      nk_property_int(ctx, "Compression:", 0, &property, 100, 10, 1);

      nk_layout_row_dynamic(ctx, 20, 1);
      nk_label(ctx, "background:", NK_TEXT_LEFT);
      nk_layout_row_dynamic(ctx, 25, 1);
      if (nk_combo_begin_color(ctx, nk_rgb_cf(bg), nk_vec2(nk_widget_width(ctx),400))) {
          nk_layout_row_dynamic(ctx, 120, 1);
          bg = nk_color_picker(ctx, bg, NK_RGBA);
          nk_layout_row_dynamic(ctx, 25, 1);
          bg.r = nk_propertyf(ctx, "#R:", 0, bg.r, 1.0f, 0.01f,0.005f);
          bg.g = nk_propertyf(ctx, "#G:", 0, bg.g, 1.0f, 0.01f,0.005f);
          bg.b = nk_propertyf(ctx, "#B:", 0, bg.b, 1.0f, 0.01f,0.005f);
          bg.a = nk_propertyf(ctx, "#A:", 0, bg.a, 1.0f, 0.01f,0.005f);
          nk_combo_end(ctx);
      }
    }
    nk_end(ctx);
  } |]

renderNuklear :: NK -> IO ()
renderNuklear (NK ptr) = do
  [C.block| void {
    nk_ctx* NK = $(void* ptr);
    nk_device* dev = &NK->ogl;

    const struct nk_draw_command* cmd;
    const struct nk_draw_index* offset = NULL;
    struct nk_buffer vbuf, ebuf;

    /* fill converting configuration */
    struct nk_convert_config config;
    static const struct nk_draw_vertex_layout_element vertex_layout[] = {
        {NK_VERTEX_POSITION, NK_FORMAT_FLOAT, NK_OFFSETOF(nk_vertex, position)},
        {NK_VERTEX_TEXCOORD, NK_FORMAT_FLOAT, NK_OFFSETOF(nk_vertex, uv)},
        {NK_VERTEX_COLOR, NK_FORMAT_R8G8B8A8, NK_OFFSETOF(nk_vertex, col)},
        {NK_VERTEX_LAYOUT_END}
    };
    NK_MEMSET(&config, 0, sizeof(config));
    config.vertex_layout = vertex_layout;
    config.vertex_size = sizeof(nk_vertex);
    config.vertex_alignment = NK_ALIGNOF(nk_vertex);
    config.null = dev->null;
    config.circle_segment_count = 22;
    config.curve_segment_count = 22;
    config.arc_segment_count = 22;
    config.global_alpha = 1.0f;
    config.shape_AA = NK_ANTI_ALIASING_ON;
    config.line_AA = NK_ANTI_ALIASING_ON;

  } |]
