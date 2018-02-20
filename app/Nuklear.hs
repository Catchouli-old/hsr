{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Nuklear where

import qualified SDL as SDL
import qualified SDL.Internal.Types as SDL
import qualified SDL.Raw as Raw
import Control.Monad.IO.Class
import Foreign
import qualified Language.C.Inline as C

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
C.include "../nuklear/nuklear_sdl_gl2.h"
C.include "<stdio.h>"

data NK = NK (Ptr ()) deriving Show
data NKAtlas = NKAtlas (Ptr ()) deriving Show

-- | Initialise nuklear
initNuklear :: SDL.Window -> IO NK
initNuklear (SDL.Window ptr) = do
  ctx <- [C.block| void* {
    SDL_Window* win = $(void* ptr);
    struct nk_context* ctx = nk_sdl_init(win);
    return ctx;
  } |]
  pure $ NK ctx

-- | Initialise atlas with default font
nuklearInitAtlas :: IO NKAtlas
nuklearInitAtlas = do
  atlas <- [C.block| void* {
    struct nk_font_atlas* atlas;
    nk_sdl_font_stash_begin(&atlas);
    nk_sdl_font_stash_end();
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
nuklearHandleEvents (NK ptr) evts = do
  [C.block| void { nk_input_begin($(void* ptr)); } |]
  mapM_ nuklearHandleEvent evts
  [C.block| void { nk_input_end($(void* ptr)); } |]

nuklearHandleEvent :: Ptr Raw.Event -> IO ()
nuklearHandleEvent evt = let ptr = castPtr evt in
  [C.block| void {
    SDL_Event* evt = $(void* ptr);
    nk_sdl_handle_event(evt);
  } |]

-- | Render
nuklearRender :: IO ()
nuklearRender = [C.block| void {
    nk_sdl_render(NK_ANTI_ALIASING_ON);
  } |]

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
    }
    nk_end(ctx);
  } |]
