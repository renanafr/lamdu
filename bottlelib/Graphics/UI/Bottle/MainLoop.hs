{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.MainLoop (mainLoopAnim, mainLoopImage, mainLoopWidget) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Lens ((^.), (%~), (.~), _1, _2)
import Control.Monad (when)
import Data.IORef
import Data.List (genericLength)
import Data.MRUMemo (memoIO)
import Data.Monoid (Monoid(..))
import Data.StateVar (($=))
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Traversable (traverse, sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.DrawingCombinators.Utils (Image, drawText, textSize)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.Widget(Widget, R)
import Graphics.UI.GLFW.Events (KeyEvent, GLFWEvent(..), eventLoop)
import Text.Printf.Mauke.TH
import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

timeBetweenInvocations ::
  IO ((Maybe NominalDiffTime -> IO a) -> IO a)
timeBetweenInvocations = do
  mLastInvocationTimeVar <- newMVar Nothing
  return $ modifyMVar mLastInvocationTimeVar . updateTime
  where
    updateTime f mLastInvocationTime = do
      currentTime <- getCurrentTime
      let
        mTimeSince =
          fmap (currentTime `diffUTCTime`) mLastInvocationTime
      result <- f mTimeSince
      return (Just currentTime, result)

mkStatser :: (Ord a, Fractional a) => Int -> IO (a -> IO (a, a, a))
mkStatser n = do
  fifo <- newIORef []
  return $ \added -> do
    modifyIORef fifo (take n . (added:))
    samples <- readIORef fifo
    return (minimum samples, avg samples, maximum samples)
  where
    avg = (/) <$> sum <*> genericLength

data SizedImage = SizedImage Image Widget.Size

instance Monoid SizedImage where
  mempty = SizedImage mempty 0
  SizedImage img0 sz0 `mappend` SizedImage img1 sz1 =
    SizedImage (img0 `mappend` img1) (max <$> sz0 <*> sz1)

scale :: Vector2 R -> SizedImage -> SizedImage
scale factor@(Vector2 w h) (SizedImage img size) =
  SizedImage (Draw.scale w h %% img) (factor * size)

-- pos must be positive
translate :: Vector2 R -> SizedImage -> SizedImage
translate pos@(Vector2 x y) (SizedImage img size) =
  SizedImage (Draw.translate (x, y) %% img) (size + pos)

getSizedImage :: SizedImage -> Image
getSizedImage (SizedImage img _) = img

placeAt :: Widget.Size -> Vector2 R -> SizedImage -> SizedImage
placeAt winSize ratio img@(SizedImage _ size) =
  translate (ratio * (winSize - size)) img

renderText :: Draw.Font -> String -> SizedImage
renderText fpsFont text = SizedImage (drawText fpsFont text) (textSize fpsFont text)

mainLoopImage ::
  Draw.Font -> (Widget.Size -> KeyEvent -> IO Bool) ->
  (Bool -> Widget.Size -> IO (Maybe Image)) -> IO a
mainLoopImage fpsFont eventHandler makeImage = GLFWUtils.withGLFW $ do
  statser <- mkStatser 20
  addDelayArg <- timeBetweenInvocations
  eventLoop $ handleEvents statser addDelayArg
  where
    windowSize = do
      (x, y) <- GLFW.getWindowDimensions
      return $ Vector2 (fromIntegral x) (fromIntegral y)

    handleEvent size (GLFWKeyEvent keyEvent) =
      eventHandler size keyEvent
    handleEvent _ GLFWWindowClose =
      error "Quit" -- TODO: Make close event
    handleEvent _ GLFWWindowRefresh = return True

    addDelayToImage statser winSize mkMImage mTimeSince = do
      mImage <- mkMImage
      let
        locate i = translate $ Vector2 0 (2*i)
        mkFpsImage (lo, avg, hi) =
          scale 20 . mconcat . zipWith locate [0..] $
          map (renderText fpsFont . $(printf "%02.02f")) [lo, avg, hi]
      fpsImage <- maybe (return mempty) (fmap mkFpsImage . statser . (1/)) mTimeSince
      return $
        mappend (getSizedImage (placeAt winSize (Vector2 1 0) fpsImage)) <$>
        mImage

    handleEvents statser addDelayArg events = do
      winSize@(Vector2 winSizeX winSizeY) <- windowSize
      anyChange <- fmap or $ traverse (handleEvent winSize) events
      GL.viewport $=
        (GL.Position 0 0,
         GL.Size (round winSizeX) (round winSizeY))
      mNewImage <-
        addDelayArg . addDelayToImage statser winSize $ makeImage anyChange winSize
      case mNewImage of
        Nothing ->
          -- TODO: If we can verify that there's sync-to-vblank, we
          -- need no sleep here
          threadDelay 10000
        Just image ->
          Draw.clearRender .
          (Draw.translate (-1, 1) %%) .
          (Draw.scale (2/winSizeX) (-2/winSizeY) %%) $
          image

mainLoopAnim ::
  Draw.Font ->
  (Widget.Size -> IO (Maybe (AnimId -> AnimId))) ->
  (Widget.Size -> KeyEvent -> IO (Maybe (AnimId -> AnimId))) ->
  (Widget.Size -> IO Anim.Frame) ->
  IO Anim.R -> IO a
mainLoopAnim font tickHandler eventHandler makeFrame getAnimationHalfLife = do
  frameStateVar <- newIORef Nothing
  let
    handleResult Nothing = return False
    handleResult (Just animIdMapping) = do
      modifyIORef frameStateVar . fmap $
        (_1 .~  0) .
        (_2 . _2 %~ Anim.mapIdentities animIdMapping)
      return True

    nextFrameState curTime size Nothing = do
      dest <- makeFrame size
      return $ Just (0, (curTime, dest))
    nextFrameState curTime size (Just (drawCount, (prevTime, prevFrame))) =
      if drawCount == 0
      then do
        dest <- makeFrame size
        animationHalfLife <- getAnimationHalfLife
        let
          elapsed = realToFrac (curTime `diffUTCTime` prevTime)
          progress = 1 - 0.5 ** (elapsed/animationHalfLife)
        return . Just $
          case Anim.nextFrame progress dest prevFrame of
            Nothing -> (drawCount + 1, (curTime, dest))
            Just newFrame -> (0 :: Int, (curTime, newFrame))
      else
        return $ Just (drawCount + 1, (curTime, prevFrame))

    makeImage forceRedraw size = do
      when forceRedraw .
        modifyIORef frameStateVar $
        Lens.mapped . _1 .~ 0
      _ <- handleResult =<< tickHandler size
      curTime <- getCurrentTime
      writeIORef frameStateVar =<<
        nextFrameState curTime size =<< readIORef frameStateVar
      fmap frameStateResult $ readIORef frameStateVar

    frameStateResult Nothing = error "No frame to draw at start??"
    frameStateResult (Just (drawCount, (_, frame)))
      | drawCount < stopAtDrawCount = Just $ Anim.draw frame
      | otherwise = Nothing
    -- A note on draw counts:
    -- When a frame is dis-similar to the previous the count resets to 0
    -- When a frame is similar and animation stops the count becomes 1
    -- We then should draw it again (for double buffering issues) at count 2
    -- And stop drawing it at count 3.
    stopAtDrawCount = 3
    imgEventHandler size event =
      handleResult =<< eventHandler size event
  mainLoopImage font imgEventHandler makeImage

compose :: [a -> a] -> a -> a
compose = foldr (.) id

mainLoopWidget :: Draw.Font -> (Widget.Size -> IO (Widget IO)) -> IO Anim.R -> IO a
mainLoopWidget font mkWidgetUnmemod getAnimationHalfLife = do
  mkWidgetRef <- newIORef =<< memoIO mkWidgetUnmemod
  let
    newWidget = writeIORef mkWidgetRef =<< memoIO mkWidgetUnmemod
    getWidget size = ($ size) =<< readIORef mkWidgetRef
    tickHandler size = do
      widget <- getWidget size
      tickResults <-
        sequenceA $ widget ^. Widget.wEventMap . E.emTickHandlers
      case tickResults of
        [] -> return Nothing
        _ -> do
          newWidget
          return . Just . compose . map (Lens.view Widget.eAnimIdMapping) $ tickResults
    eventHandler size event = do
      widget <- getWidget size
      mAnimIdMapping <-
        (traverse . fmap) (Lens.view Widget.eAnimIdMapping) .
        E.lookup event $ widget ^. Widget.wEventMap
      case mAnimIdMapping of
        Nothing -> return ()
        Just _ -> newWidget
      return mAnimIdMapping
    mkFrame size = Lens.view Widget.wFrame <$> getWidget size
  mainLoopAnim font tickHandler eventHandler mkFrame getAnimationHalfLife
