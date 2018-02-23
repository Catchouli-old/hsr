{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Nuklear where

import qualified SDL as SDL
import qualified SDL.Internal.Types as SDL
import qualified SDL.Raw as Raw
import Control.Monad.IO.Class
import Foreign
import qualified Language.C.Inline as C

C.context (mappend C.baseCtx C.funCtx)

C.verbatim "#define NK_INCLUDE_FIXED_TYPES"
C.verbatim "#define NK_INCLUDE_STANDARD_IO"
C.verbatim "#define NK_INCLUDE_STANDARD_VARARGS"
C.verbatim "#define NK_INCLUDE_DEFAULT_ALLOCATOR"
C.verbatim "#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT"
C.verbatim "#define NK_INCLUDE_FONT_BAKING"
C.verbatim "#define NK_INCLUDE_DEFAULT_FONT"
C.verbatim "#define NK_IMPLEMENTATION"
C.verbatim "#define NK_SDL_GL2_IMPLEMENTATION"

C.include "../nuklear/nuklear.h"
C.include "<stdio.h>"
C.include "<SDL2/SDL.h>"
C.include "<SDL2/SDL_opengl.h>"

C.verbatim "struct nk_sdl_device {"
C.verbatim "    struct nk_buffer cmds;"
C.verbatim "    struct nk_draw_null_texture null;"
C.verbatim "    GLuint font_tex;"
C.verbatim "};"

C.verbatim "struct nk_sdl_vertex {"
C.verbatim "    float position[2];"
C.verbatim "    float uv[2];"
C.verbatim "    nk_byte col[4];"
C.verbatim "};"

C.verbatim "static struct nk_sdl {"
C.verbatim "    SDL_Window *win;"
C.verbatim "    struct nk_sdl_device ogl;"
C.verbatim "    struct nk_context ctx;"
C.verbatim "    struct nk_font_atlas atlas;"
C.verbatim "} sdl;"

data NK = NK { nkCtx :: Ptr ()
             }
data NKAtlas = NKAtlas (Ptr ())

C.verbatim "static void"
C.verbatim "nk_sdl_clipboard_paste(nk_handle usr, struct nk_text_edit *edit)"
C.verbatim "{"
C.verbatim "    const char *text = SDL_GetClipboardText();"
C.verbatim "    printf(\"pasting text: %s\", text);"
C.verbatim "    if (text) nk_textedit_paste(edit, text, nk_strlen(text));"
C.verbatim "    (void)usr;"
C.verbatim "}"

C.verbatim "static void"
C.verbatim "nk_sdl_clipboard_copy(nk_handle usr, const char *text, int len)"
C.verbatim "{"
C.verbatim "    char *str = 0;"
C.verbatim "    (void)usr;"
C.verbatim "    if (!len) return;"
C.verbatim "    str = (char*)malloc((size_t)len+1);"
C.verbatim "    if (!str) return;"
C.verbatim "    memcpy(str, text, (size_t)len);"
C.verbatim "    str[len] = 0;"
C.verbatim "    printf(\"copying text: %s\", str);"
C.verbatim "    SDL_SetClipboardText(str);"
C.verbatim "    free(str);"
C.verbatim "}"

-- | Initialise nuklear
initNuklear :: SDL.Window -> IO NK
initNuklear (SDL.Window ptr) = do
  ctx <- [C.block| void* {
    SDL_Window* win = $(void* ptr);
    sdl.win = win;
    nk_init_default(&sdl.ctx, 0);
    sdl.ctx.clip.copy = nk_sdl_clipboard_copy;
    sdl.ctx.clip.paste = nk_sdl_clipboard_paste;
    sdl.ctx.clip.userdata = nk_handle_ptr(0);
    nk_buffer_init_default(&sdl.ogl.cmds);
    return &sdl.ctx;
  } |]
  pure $ NK
    { nkCtx = ctx
    }

-- | Initialise atlas with default font
nuklearInitAtlas :: IO NKAtlas
nuklearInitAtlas = do
  atlas <- [C.block| void* {
    struct nk_font_atlas* atlas;

    // Font stash begin
    nk_font_atlas_init_default(&sdl.atlas);
    nk_font_atlas_begin(&sdl.atlas);
    atlas = &sdl.atlas;

    // font stash end
    const void *image; int w, h;
    image = nk_font_atlas_bake(&sdl.atlas, &w, &h, NK_FONT_ATLAS_RGBA32);

    //nk_sdl_device_upload_atlas(image, w, h);
    struct nk_sdl_device *dev = &sdl.ogl;
    glGenTextures(1, &dev->font_tex);
    glBindTexture(GL_TEXTURE_2D, dev->font_tex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, (GLsizei)w, (GLsizei)h, 0,
                GL_RGBA, GL_UNSIGNED_BYTE, image);

    nk_font_atlas_end(&sdl.atlas, nk_handle_id((int)sdl.ogl.font_tex), &sdl.ogl.null);
    if (sdl.atlas.default_font)
        nk_style_set_font(&sdl.ctx, &sdl.atlas.default_font->handle);

    return atlas;
  } |]
  pure $ NKAtlas atlas

-- | pollEvent except it also returns the raw events
pollEvent' :: MonadIO m => m (Maybe (SDL.Event, Ptr Raw.Event))
pollEvent' = liftIO $ alloca $ \e -> do
  n <- Raw.pollEvent e
  if n == 0
     then return Nothing
     else do
       converted <- (peek e >>= SDL.convertRaw)
       pure $ Just $ (converted, e)

-- | pollEvents except it also returns the raw events
pollEvents' :: (MonadIO m) => m [(SDL.Event, Ptr Raw.Event)]
pollEvents' =
  do e <- pollEvent'
     case e of
       Nothing -> return []
       Just e' -> (e' :) <$> pollEvents'

-- | Handle events
nuklearHandleEvents :: NK -> [Ptr Raw.Event] -> IO ()
nuklearHandleEvents nk@(NK ptr) evts = do
  [C.block| void { nk_input_begin($(void* ptr)); } |]
  mapM_ (nuklearHandleEvent nk) evts
  [C.block| void { nk_input_end($(void* ptr)); } |]

nuklearHandleEvent :: NK -> Ptr Raw.Event -> IO ()
nuklearHandleEvent (NK _) evt = let ptr = castPtr evt in
  [C.block| void {
    SDL_Event* evt = $(void* ptr);

    //nk_sdl_handle_event(evt);
    struct nk_context *ctx = &sdl.ctx;

    /* optional grabbing behavior */
    if (ctx->input.mouse.grab) {
        SDL_SetRelativeMouseMode(SDL_TRUE);
        ctx->input.mouse.grab = 0;
    } else if (ctx->input.mouse.ungrab) {
        int x = (int)ctx->input.mouse.prev.x, y = (int)ctx->input.mouse.prev.y;
        SDL_SetRelativeMouseMode(SDL_FALSE);
        SDL_WarpMouseInWindow(sdl.win, x, y);
        ctx->input.mouse.ungrab = 0;
    }
    if (evt->type == SDL_KEYUP || evt->type == SDL_KEYDOWN) {
        /* key events */
        int down = evt->type == SDL_KEYDOWN;
        const Uint8* state = SDL_GetKeyboardState(0);
        SDL_Keycode sym = evt->key.keysym.sym;
        if (sym == SDLK_RSHIFT || sym == SDLK_LSHIFT)
            nk_input_key(ctx, NK_KEY_SHIFT, down);
        else if (sym == SDLK_DELETE)
            nk_input_key(ctx, NK_KEY_DEL, down);
        else if (sym == SDLK_RETURN)
            nk_input_key(ctx, NK_KEY_ENTER, down);
        else if (sym == SDLK_TAB)
            nk_input_key(ctx, NK_KEY_TAB, down);
        else if (sym == SDLK_BACKSPACE)
            nk_input_key(ctx, NK_KEY_BACKSPACE, down);
        else if (sym == SDLK_HOME) {
            nk_input_key(ctx, NK_KEY_TEXT_START, down);
            nk_input_key(ctx, NK_KEY_SCROLL_START, down);
        } else if (sym == SDLK_END) {
            nk_input_key(ctx, NK_KEY_TEXT_END, down);
            nk_input_key(ctx, NK_KEY_SCROLL_END, down);
        } else if (sym == SDLK_PAGEDOWN) {
            nk_input_key(ctx, NK_KEY_SCROLL_DOWN, down);
        } else if (sym == SDLK_PAGEUP) {
            nk_input_key(ctx, NK_KEY_SCROLL_UP, down);
        } else if (sym == SDLK_z)
            nk_input_key(ctx, NK_KEY_TEXT_UNDO, down && state[SDL_SCANCODE_LCTRL]);
        else if (sym == SDLK_r)
            nk_input_key(ctx, NK_KEY_TEXT_REDO, down && state[SDL_SCANCODE_LCTRL]);
        else if (sym == SDLK_c)
            nk_input_key(ctx, NK_KEY_COPY, down && state[SDL_SCANCODE_LCTRL]);
        else if (sym == SDLK_v)
            nk_input_key(ctx, NK_KEY_PASTE, down && state[SDL_SCANCODE_LCTRL]);
        else if (sym == SDLK_x)
            nk_input_key(ctx, NK_KEY_CUT, down && state[SDL_SCANCODE_LCTRL]);
        else if (sym == SDLK_b)
            nk_input_key(ctx, NK_KEY_TEXT_LINE_START, down && state[SDL_SCANCODE_LCTRL]);
        else if (sym == SDLK_e)
            nk_input_key(ctx, NK_KEY_TEXT_LINE_END, down && state[SDL_SCANCODE_LCTRL]);
        else if (sym == SDLK_UP)
            nk_input_key(ctx, NK_KEY_UP, down);
        else if (sym == SDLK_DOWN)
            nk_input_key(ctx, NK_KEY_DOWN, down);
        else if (sym == SDLK_LEFT) {
            if (state[SDL_SCANCODE_LCTRL])
                nk_input_key(ctx, NK_KEY_TEXT_WORD_LEFT, down);
            else nk_input_key(ctx, NK_KEY_LEFT, down);
        } else if (sym == SDLK_RIGHT) {
            if (state[SDL_SCANCODE_LCTRL])
                nk_input_key(ctx, NK_KEY_TEXT_WORD_RIGHT, down);
            else nk_input_key(ctx, NK_KEY_RIGHT, down);
        } else return;
        return;
    } else if (evt->type == SDL_MOUSEBUTTONDOWN || evt->type == SDL_MOUSEBUTTONUP) {
        /* mouse button */
        int down = evt->type == SDL_MOUSEBUTTONDOWN;
        const int x = evt->button.x, y = evt->button.y;
        if (evt->button.button == SDL_BUTTON_LEFT) {
            if (evt->button.clicks > 1)
                nk_input_button(ctx, NK_BUTTON_DOUBLE, x, y, down);
            nk_input_button(ctx, NK_BUTTON_LEFT, x, y, down);
        } else if (evt->button.button == SDL_BUTTON_MIDDLE)
            nk_input_button(ctx, NK_BUTTON_MIDDLE, x, y, down);
        else if (evt->button.button == SDL_BUTTON_RIGHT)
            nk_input_button(ctx, NK_BUTTON_RIGHT, x, y, down);
        return;
    } else if (evt->type == SDL_MOUSEMOTION) {
        /* mouse motion */
        if (ctx->input.mouse.grabbed) {
            int x = (int)ctx->input.mouse.prev.x, y = (int)ctx->input.mouse.prev.y;
            nk_input_motion(ctx, x + evt->motion.xrel, y + evt->motion.yrel);
        } else nk_input_motion(ctx, evt->motion.x, evt->motion.y);
        return;
    } else if (evt->type == SDL_TEXTINPUT) {
        /* text input */
        nk_glyph glyph;
        memcpy(glyph, evt->text.text, NK_UTF_SIZE);
        nk_input_glyph(ctx, glyph);
        return;
    } else if (evt->type == SDL_MOUSEWHEEL) {
        /* mouse wheel */
        nk_input_scroll(ctx,nk_vec2((float)evt->wheel.x,(float)evt->wheel.y));
        return;
    }
  } |]

-- | Render
nuklearRender :: IO ()
nuklearRender = [C.block| void {
    //nk_sdl_render(NK_ANTI_ALIASING_ON);
    /* setup global state */
    struct nk_sdl_device *dev = &sdl.ogl;
    int width, height;
    int display_width, display_height;
    struct nk_vec2 scale;

    SDL_GetWindowSize(sdl.win, &width, &height);
    SDL_GL_GetDrawableSize(sdl.win, &display_width, &display_height);
    scale.x = (float)display_width/(float)width;
    scale.y = (float)display_height/(float)height;

    glPushAttrib(GL_ENABLE_BIT|GL_COLOR_BUFFER_BIT|GL_TRANSFORM_BIT);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_SCISSOR_TEST);
    glEnable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    /* setup viewport/project */
    glViewport(0,0,(GLsizei)display_width,(GLsizei)display_height);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(0.0f, width, height, 0.0f, -1.0f, 1.0f);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    {
        GLsizei vs = sizeof(struct nk_sdl_vertex);
        size_t vp = offsetof(struct nk_sdl_vertex, position);
        size_t vt = offsetof(struct nk_sdl_vertex, uv);
        size_t vc = offsetof(struct nk_sdl_vertex, col);

        /* convert from command queue into draw list and draw to screen */
        const struct nk_draw_command *cmd;
        const nk_draw_index *offset = NULL;
        struct nk_buffer vbuf, ebuf;

        /* fill converting configuration */
        struct nk_convert_config config;
        static const struct nk_draw_vertex_layout_element vertex_layout[] = {
            {NK_VERTEX_POSITION, NK_FORMAT_FLOAT, NK_OFFSETOF(struct nk_sdl_vertex, position)},
            {NK_VERTEX_TEXCOORD, NK_FORMAT_FLOAT, NK_OFFSETOF(struct nk_sdl_vertex, uv)},
            {NK_VERTEX_COLOR, NK_FORMAT_R8G8B8A8, NK_OFFSETOF(struct nk_sdl_vertex, col)},
            {NK_VERTEX_LAYOUT_END}
        };
        NK_MEMSET(&config, 0, sizeof(config));
        config.vertex_layout = vertex_layout;
        config.vertex_size = sizeof(struct nk_sdl_vertex);
        config.vertex_alignment = NK_ALIGNOF(struct nk_sdl_vertex);
        config.null = dev->null;
        config.circle_segment_count = 22;
        config.curve_segment_count = 22;
        config.arc_segment_count = 22;
        config.global_alpha = 1.0f;
        config.shape_AA = NK_ANTI_ALIASING_ON;
        config.line_AA = NK_ANTI_ALIASING_ON;

        /* convert shapes into vertexes */
        nk_buffer_init_default(&vbuf);
        nk_buffer_init_default(&ebuf);
        nk_convert(&sdl.ctx, &dev->cmds, &vbuf, &ebuf, &config);

        /* setup vertex buffer pointer */
        {const void *vertices = nk_buffer_memory_const(&vbuf);
        glVertexPointer(2, GL_FLOAT, vs, (const void*)((const nk_byte*)vertices + vp));
        glTexCoordPointer(2, GL_FLOAT, vs, (const void*)((const nk_byte*)vertices + vt));
        glColorPointer(4, GL_UNSIGNED_BYTE, vs, (const void*)((const nk_byte*)vertices + vc));}

        /* iterate over and execute each draw command */
        offset = (const nk_draw_index*)nk_buffer_memory_const(&ebuf);
        nk_draw_foreach(cmd, &sdl.ctx, &dev->cmds)
        {
            if (!cmd->elem_count) continue;
            glBindTexture(GL_TEXTURE_2D, (GLuint)cmd->texture.id);
            glScissor(
                (GLint)(cmd->clip_rect.x * scale.x),
                (GLint)((height - (GLint)(cmd->clip_rect.y + cmd->clip_rect.h)) * scale.y),
                (GLint)(cmd->clip_rect.w * scale.x),
                (GLint)(cmd->clip_rect.h * scale.y));
            glDrawElements(GL_TRIANGLES, (GLsizei)cmd->elem_count, GL_UNSIGNED_SHORT, offset);
            offset += cmd->elem_count;
        }
        nk_clear(&sdl.ctx);
        nk_buffer_free(&vbuf);
        nk_buffer_free(&ebuf);
    }

    /* default OpenGL state */
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);

    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_SCISSOR_TEST);
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);

    glBindTexture(GL_TEXTURE_2D, 0);
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glPopAttrib();
  } |]

nuklearShutdown :: IO ()
nuklearShutdown = [C.block| void {
    struct nk_sdl_device *dev = &sdl.ogl;
    nk_font_atlas_clear(&sdl.atlas);
    nk_free(&sdl.ctx);
    glDeleteTextures(1, &dev->font_tex);
    nk_buffer_free(&dev->cmds);
    memset(&sdl, 0, sizeof(sdl));
  } |]

C.verbatim "int textLen;"
C.verbatim "char textBuf[1024];"

test :: NK -> IO ()
test (NK ptr) = do
  [C.block| void {
    struct nk_context* ctx = $(void* ptr);

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

//nk_edit_string(struct nk_context *ctx, nk_flags flags,
    //char *memory, int *len, int max, nk_plugin_filter filter)
      nk_edit_string(ctx, NK_EDIT_FIELD, textBuf, &textLen, 1024, nk_filter_default);
    }
    nk_end(ctx);
  } |]
