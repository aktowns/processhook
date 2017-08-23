{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
-- Poc wallhack/bright wall for q3
module Lib where

import           Control.Applicative             (liftA2)
import           Control.Monad                   (forM_, replicateM, unless,
                                                  when)
import           Control.Monad.Loops             (whileM)
import           Data.List                       (minimumBy, nub)
import           Foreign.C.String                (CString, withCString)
import           Foreign.C.Types
import           Foreign.Ptr                     (FunPtr, Ptr, castFunPtrToPtr,
                                                  castPtr, nullPtr, plusPtr)
import           Foreign.Storable
import           System.Posix.DynamicLinker      (DL, RTLDFlags (RTLD_LAZY),
                                                  dlopen, dlsym)
import           System.Posix.DynamicLinker.Prim

-- $1 = {valid = qtrue, snapFlags = 4, serverTime = 19200, messageNum = 1514, deltaNum = 1513, ping = 0,
--   areamask = "\376", '\000' <repeats 30 times>, cmdNum = 0, ps = {commandTime = 19162, pm_type = 0, bobCycle = 0,
--     pm_flags = 16, pm_time = 0, origin = {287.039276, 2376.87329, 56.125}, velocity = {0, 0, 0}, weaponTime = 0,
--     gravity = 800, speed = 320, delta_angles = {0, 57344, 0}, groundEntityNum = 1022, legsTimer = 0, legsAnim = 150,
--     torsoTimer = 0, torsoAnim = 139, movementDir = 4, grapplePoint = {0, 0, 0}, eFlags = 4100, eventSequence = 25, events = {
--       1, 7}, eventParms = {0, 0}, externalEvent = 0, externalEventParm = 0, externalEventTime = 0, clientNum = 1, weapon = 5,
--     weaponstate = 0, viewangles = {0, -45, 0}, viewheight = 26, damageEvent = 0, damageYaw = 0, damagePitch = 0,
--     damageCount = 0, stats = {107, 0, 38, 0, 0, 0, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0}, persistant = {0, 0, 16384, 0, 1,
--       0 <repeats 11 times>}, powerups = {0 <repeats 16 times>}, ammo = {0, -1, 100, 0, 0, 10, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0},
--     generic1 = 0, loopSound = 0, jumppad_ent = 0, ping = 0, pmove_framecount = 0, jumppad_frame = 0, entityEventSequence = 0},
--   numEntities = 15, parseEntitiesNum = 23591, serverCommandNum = 2}

countM :: (Applicative m) => Int -> (Int -> m a) -> m [a]
countM cnt0 f = loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure []
        | otherwise = liftA2 (:) (f (cnt0 - cnt)) (loop (cnt - 1))

data Vec3 = Vec3 { x :: CFloat, y :: CFloat, z :: CFloat } deriving (Eq, Ord, Show)
emptyVec = Vec3 {x = 0, y = 0, z = 0}

instance Storable Vec3 where
  sizeOf _ = 12
  alignment = sizeOf
  poke ptr Vec3 {x, y, z} = do
    pokeByteOff ptr 0 x
    pokeByteOff ptr 4 y
    pokeByteOff ptr 8 z
  peek ptr = do
    x <- peekByteOff ptr 0
    y <- peekByteOff ptr 4
    z <- peekByteOff ptr 8
    return Vec3 { x = x, y = y, z = z }

data PlayerState = PlayerState { commandTime :: CInt
                               , pmType      :: CInt
                               , bobCycle    :: CInt
                               , pmFlags     :: CInt
                               , pmTime      :: CInt
                               , origin      :: Vec3
                               , velocity    :: Vec3
                               , weaponTime  :: CInt
                               , gravity     :: CInt
                               , speed       :: CInt
                               } deriving (Eq, Ord, Show)

instance Storable PlayerState where
  sizeOf _ = 468
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = do
    ct <- peekByteOff ptr 0
    pty <- peekByteOff ptr 4
    bob <- peekByteOff ptr 8
    pf <- peekByteOff ptr 12
    pti <- peekByteOff ptr 16
    orig <- peekByteOff ptr 20
    velo <- peekByteOff ptr 32
    wt <- peekByteOff ptr 44
    grav <- peekByteOff ptr 48
    speed <- peekByteOff ptr 52
    return PlayerState { commandTime = ct
                       , pmType = pty
                       , bobCycle = bob
                       , pmFlags = pf
                       , pmTime = pti
                       , origin = orig
                       , velocity = velo
                       , weaponTime = wt
                       , gravity = grav
                       , speed = speed }

data Trajectory = Trajectory { trType     :: CInt
                             , trTime     :: CInt
                             , trDuration :: CInt
                             , trBase     :: Vec3
                             , trDelta    :: Vec3
                             } deriving (Eq, Ord, Show)

instance Storable Trajectory where
  sizeOf _ = 36
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = do
    tty <- peekByteOff ptr 0
    tti <- peekByteOff ptr 4
    tdu <- peekByteOff ptr 8
    tba <- peekByteOff ptr 12
    tde <- peekByteOff ptr 24
    return Trajectory { trType = tty
                      , trTime = tti
                      , trDuration = tdu
                      , trBase = tba
                      , trDelta = tde }

data EntityState = EntityState { entityNumber          :: CInt
                               , entityEType           :: CInt
                               , entityEFlags          :: CInt
                               , entityPos             :: Trajectory
                               , entityAPos            :: Trajectory
                               , entityTime            :: CInt
                               , entityTime2           :: CInt
                               , entityOrigin          :: Vec3
                               , entityOrigin2         :: Vec3
                               , entityAngles          :: Vec3
                               , entityAngles2         :: Vec3
                               , entityOtherEntityNum  :: CInt
                               , entityOtherEntityNum2 :: CInt
                               , entityGroundEntityNum :: CInt
                               , entityConstantLight   :: CInt
                               , entityLoopSound       :: CInt
                               , entityModelIndex      :: CInt
                               , entityModelIndex2     :: CInt
                               , entityClientNum       :: CInt
                               , entityFrame           :: CInt
                               , entitySolid           :: CInt
                               , entityEvent           :: CInt
                               , entityEventParm       :: CInt
                               } deriving (Eq, Ord, Show)

instance Storable EntityState where
  sizeOf _ = 208
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = do
    enu <- peekByteOff ptr 0
    eet <- peekByteOff ptr 4
    eef <- peekByteOff ptr 8
    epo <- peekByteOff ptr 12
    eap <- peekByteOff ptr 48
    eti <- peekByteOff ptr 64
    eti2 <- peekByteOff ptr 68
    eto <- peekByteOff ptr 72
    eto2 <- peekByteOff ptr 84
    eta <- peekByteOff ptr 96
    eta2 <- peekByteOff ptr 108
    return EntityState { entityNumber = enu
                       , entityEType = eet
                       , entityEFlags = eef
                       , entityPos = epo
                       , entityAPos = eap
                       , entityTime = eti
                       , entityTime2 = eti2
                       , entityOrigin = eto
                       , entityOrigin2 = eto2
                       , entityAngles = eta
                       , entityAngles2 = eta2
                       }

data Snapshot = Snapshot { snapValid            :: CInt
                         , snapSnapFlags        :: CInt
                         , snapServerTime       :: CInt
                         , snapMessageNum       :: CInt
                         , snapDeltaNum         :: CInt
                         , snapPing             :: CInt
                         , snapAreamask         :: [CInt]
                         , snapCmdNum           :: CInt
                         , snapPs               :: PlayerState
                         , snapNumEntities      :: CInt
                         , snapParseEntitiesNum :: CInt
                         , snapServerCommandNum :: CInt
                         } deriving (Eq, Ord, Show)

instance Storable Snapshot where
  sizeOf _ = 540
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = do
    va <- peekByteOff ptr 0
    sn <- peekByteOff ptr 4
    se <- peekByteOff ptr 8
    me <- peekByteOff ptr 12
    de <- peekByteOff ptr 16
    pi <- peekByteOff ptr 20
    ar <- countM 32 $ \i -> peekByteOff ptr (24 + i)
    cm <- peekByteOff ptr 56
    ps <- peekByteOff ptr 60
    ne <- peekByteOff ptr 528
    pe <- peekByteOff ptr 532
    sc <- peekByteOff ptr 536
    return Snapshot { snapValid = va
                    , snapSnapFlags = sn
                    , snapServerTime = se
                    , snapMessageNum = me
                    , snapDeltaNum = de
                    , snapPing = pi
                    , snapAreamask = ar
                    , snapCmdNum = cm
                    , snapPs = ps
                    , snapNumEntities = ne
                    , snapParseEntitiesNum = pe
                    , snapServerCommandNum = sc
                    }

data GameState = GameState {} deriving (Eq, Ord, Show)
instance Storable GameState where
  sizeOf _ = 20100
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = return GameState {}

data UserCmd = UserCmd {} deriving (Eq, Ord, Show)
instance Storable UserCmd where
  sizeOf _ = 24
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = return UserCmd {}

data OutPacket = OutPacket {} deriving (Eq, Ord, Show)
instance Storable OutPacket where
  sizeOf _ = 12
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = return OutPacket {}

data ClientActive = ClientActive { timeoutCount         :: CInt
                                 , snap                 :: Snapshot
                                 , serverTime           :: CInt
                                 , oldServerTime        :: CInt
                                 , oldFrameServerTime   :: CInt
                                 , serverTimeDelta      :: CInt
                                 , extrapolatedSnapshot :: CInt
                                 , newSnapshots         :: CInt
                                 , gameState            :: GameState
                                 , mapName              :: [CChar]
                                 , parseEntitiesNum     :: CInt
                                 , mouseDx              :: [CInt]
                                 , mouseDy              :: [CInt]
                                 , mouseIndex           :: CInt
                                 , joystickAxis         :: [CInt]
                                 , cgameUserCmdValue    :: CInt
                                 , cgameSensitivity     :: CFloat
                                 , cmds                 :: [UserCmd]
                                 , cmdNumber            :: CInt
                                 , outPackets           :: [OutPacket]
                                 , viewangles           :: Vec3
                                 , serverId             :: CInt
                                 , snapshots            :: [Snapshot]
                                -- , entityBaselines      :: [EntityState]
                                -- , parseEntities        :: [EntityState]
                                 } deriving (Eq, Ord, Show)

newtype Entities = Entities [EntityState] deriving (Show)
instance Storable Entities where
  sizeOf _ = 1703936
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = do
    -- pe <- countM 8192 $ \i -> peekByteOff ptr $ 253040 + (208 * i)
    pe <- countM 8192 $ \i -> peekByteOff ptr $ 253040 + (208 * i)
    return $ Entities pe

instance Storable ClientActive where
  sizeOf _ = 1956976
  alignment = sizeOf
  poke ptr v = pokeByteOff ptr 22752 (viewangles v)

  peek ptr = do
    ti <- peekByteOff ptr 0
    sn <- peekByteOff ptr 4
    st <- peekByteOff ptr 544
    ost <- peekByteOff ptr 548
    ofst <- peekByteOff ptr 552
    std <- peekByteOff ptr 556
    es <- peekByteOff ptr 560
    ns <- peekByteOff ptr 564
    gs <- peekByteOff ptr 568
    mn <- countM 64 $ \i -> peekByteOff ptr $ 20668 + (4 * i)
    pen <- peekByteOff ptr 20732
    mdx <- countM 2 $ \i -> peekByteOff ptr $ 20736 + (4 * i)
    mdy <- countM 2 $ \i -> peekByteOff ptr $ 20744 + (4 * i)
    mi <- peekByteOff ptr 20752
    ja <- countM 16 $ \i -> peekByteOff ptr $ 20756 + (4 * i)
    cucv <- peekByteOff ptr 20820
    cs <- peekByteOff ptr 20824
    cmds <- countM 64 $ \i -> peekByteOff ptr $ 20828 + (24 * i)
    cn <- peekByteOff ptr 22364
    op <- countM 32 $ \i -> peekByteOff ptr $ 22368 + (12 * i)
    va <- peekByteOff ptr 22752
    si <- peekByteOff ptr 22764
    snaps <- countM 32 $ \i -> peekByteOff ptr $ 22768 + (540 * i)
    --ebl <- countM 1024 $ \i -> peekByteOff ptr $ 40048 + (208 * i)
    --pe <- countM 8192 $ \i -> peekByteOff ptr $ 253040 + (208 * i)
    return ClientActive { timeoutCount = ti
                        , snap = sn
                        , serverTime = st
                        , oldServerTime = ost
                        , oldFrameServerTime = ofst
                        , serverTimeDelta = std
                        , extrapolatedSnapshot = es
                        , newSnapshots = ns
                        , gameState = gs
                        , mapName = mn
                        , parseEntitiesNum = pen
                        , mouseDx = mdx
                        , mouseDy = mdy
                        , mouseIndex = mi
                        , joystickAxis = ja
                        , cgameUserCmdValue = cucv
                        , cgameSensitivity = cs
                        , cmds = cmds
                        , cmdNumber = cn
                        , outPackets = op
                        , viewangles = va
                        , serverId = si
                        , snapshots = snaps
                       -- , entityBaselines = ebl
                       -- , parseEntities = pe
                        }

data Key = Key { down    :: Bool
               , repeats :: CInt
               , binding :: CString
               } deriving (Show, Eq, Ord)

instance Storable Key where
  sizeOf _ = 16
  alignment = sizeOf
  poke _ _ = return ()
  peek ptr = do
    d <- peekByteOff ptr 0
    r <- peekByteOff ptr 4
    b <- peekByteOff ptr 8
    return Key { down = d
               , repeats = r
               , binding = b }

-- SDL marshallers
foreign import ccall "dynamic" sdlInitCall            :: FunPtr (CUInt -> IO CInt) -> CUInt -> IO CInt
foreign import ccall "dynamic" sdlSetRenderTargetCall :: FunPtr (Ptr () -> Ptr () -> IO CInt) -> Ptr () -> Ptr () -> IO CInt
-- OpenGL marshallers
foreign import ccall "dynamic" glClearCall           :: FunPtr (CInt -> IO ()) -> CInt -> IO ()
foreign import ccall "dynamic" glFlushCall           :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" glTexCoordPointerCall :: FunPtr (CInt -> CInt -> CInt -> Ptr () -> IO ()) -> CInt -> CInt -> CInt -> Ptr () -> IO ()
foreign import ccall "dynamic" glDrawElementsCall    :: FunPtr (CInt -> CInt -> CInt -> Ptr () -> IO ()) -> CInt -> CInt -> CInt -> Ptr () -> IO ()

-- SDL hooks
foreign export ccall "SDL_Init" sdlInit                       :: CUInt -> IO CInt
foreign export ccall "SDL_SetRenderTarget" sdlSetRenderTarget :: Ptr () -> Ptr () -> IO CInt
-- OpenGL hooks
foreign export ccall glClear           :: CInt -> IO ()
foreign export ccall glFlush           :: IO ()
foreign export ccall glTexCoordPointer :: CInt -> CInt -> CInt -> Ptr () -> IO ()
foreign export ccall glDrawElements    :: CInt -> CInt -> CInt -> Ptr () -> IO ()

foreign import ccall glDepthFunc    :: CUInt -> IO ()
foreign import ccall glTexEnvi      :: CUInt -> CUInt -> CInt -> IO ()
foreign import ccall glPushMatrix   :: IO ()
foreign import ccall glPopMatrix    :: IO ()
foreign import ccall glLoadIdentity :: IO ()
foreign import ccall glDisable      :: CUInt -> IO ()
foreign import ccall glEnable       :: CUInt -> IO ()
foreign import ccall glColor4f      :: Float -> Float -> Float -> Float -> IO ()
foreign import ccall glColor3f      :: Float -> Float -> Float -> IO ()
foreign import ccall glRasterPos2f  :: Float -> Float -> IO ()
foreign import ccall glCallLists    :: Int -> CUInt -> Ptr a -> IO ()

foreign import ccall "dynamic" cvarSetCall :: FunPtr (CString -> CString -> Bool -> IO (Ptr ())) -> CString -> CString -> Bool -> IO (Ptr ())
foreign import ccall "dynamic" clConsolePrintCall :: FunPtr (CString -> IO ()) -> CString -> IO ()
foreign import ccall "Cvar_Set2" cvarSet  :: CString -> CString -> Bool -> IO (Ptr ())
foreign import ccall "CL_MouseEvent" clMouseEvent :: CInt -> CInt -> CInt -> IO ()

foreign import ccall "his_locked" isLocked :: IO Bool
foreign import ccall "hlock" lock :: IO ()
foreign import ccall "hunlock" unlock :: IO ()

glTexture2D      = 0x0DE1
glUnsignedByte   = 0x1401
glTextureEnv     = 0x2300
glTextureEnvMode = 0x2200
glBlend          = 0x0BE2
glDecal          = 0x2101
glModulate       = 0x2100
glTriangles      = 0x0004
glColourMaterial = 0x0B57
glQuads          = 0x0007
glAlways         = 0x0207
glLEqual         = 0x0203
glFloat          = 0x1406
glDepthTest      = 0x0B71

libSdl = dlopen "/usr/lib/libSDL2-2.0.so.0" [RTLD_LAZY]
libGl  = dlopen "/usr/lib/libGL.so" [RTLD_LAZY]
host   = DLHandle <$> c_dlopen nullPtr (packRTLDFlags [RTLD_LAZY])

type RGBA = (Float, Float, Float, Float)
type CoOrd = (Float, Float)

kTab = 9
kEnter = 13
kCmd = 128

etGeneral = 0
etPlayer = 1

checkKey :: Int -> IO Bool
checkKey key = do
  self <- host
  keys <- dlsym self "keys"
  let ptr = plusPtr (castFunPtrToPtr keys) $ 16 * key

  k <- peek ptr
  return $ down k

drawText :: String -> CoOrd -> RGBA -> IO ()
drawText text (x,y) (r,g,b,a) = do
  glPushMatrix
  glLoadIdentity
  glDisable glTexture2D
  glColor4f r g b a
  glRasterPos2f x y
  withCString text $ \cstr -> glCallLists (length text) glUnsignedByte cstr
  glEnable glTexture2D
  glPopMatrix

glDrawElementsSym :: IO (FunPtr a)
glDrawElementsSym = do
  dl <- libGl
  dlsym dl "glDrawElements"

cprint :: String -> IO ()
cprint str = do
  self <- host
  sym <- dlsym self "CL_ConsolePrint"

  withCString str $ \cstr -> clConsolePrintCall sym cstr

clientActive :: IO (Ptr ClientActive)
clientActive = do
  self <- host
  sym <- dlsym self "cl"

  return $ castFunPtrToPtr sym

entities :: IO Entities
entities = do
  self <- host
  sym <- dlsym self "cl"

  let ptr = castFunPtrToPtr sym
  print ptr
  peek ptr

-- So in order for us to aim at the hard of the enemy we need to
-- calculate the angle relative to our player.
-- Given our players origin, our targets origin, our eye position
-- and the targets head position first we need to subtract our
-- eye position from the targets head position to give us the position
-- of our target relative to our head
-- next we need the pitch and yaw of our player to calculate the distance to turn
--magnitude :: Vec3 -> Int
--magnitude Vec3 {x, y, z} = sqrt (x * x + y * y + z * z)
--
--calc :: Vec3 -> Vec3 ()
--calc viewAngle target = ()
--  where
--    targetYaw   = atan $ (y target) / (z target)
--    targetPitch = acos $ (z target) / magnitude target
--    originYaw   = atan $ (y viewAngle) / (z viewAngle)       -- arctan(y/z)
--    originPitch = acos $ (z viewAngle) / magnitude viewAngle -- arccos(z/hypotenuse)

cvar :: String -> String -> IO ()
cvar k v = do
  --self <- host
  --sym <- dlsym self "Cvar_Set2"

  withCString k $ \ck -> withCString v $ \cv -> do
      --cvarSetCall sym ck cv True
      cvarSet ck cv True
      return ()

glDrawElements :: CInt -> CInt -> CInt -> Ptr () -> IO ()
glDrawElements mode count typ indices = do
  origDrawEl <- glDrawElementsSym

  !locked <- isLocked
  unless locked $ do
    !_ <- lock
    !key <- checkKey kEnter
    when key $ do
      (Entities e) <- entities
      let ee = filter (\e -> entityEType e == etPlayer && entityOrigin e /= emptyVec) e
      print $ nub $ map (\e -> entityEType e) e
      when (length ee > 0) $ do
        cprint "Hack Active"
        putStrLn "---\n\n"
        putStrLn $ "Aiming at " ++ show (length ee)
        putStrLn "\n\n---"
        client <- clientActive
        let enemy = minimumBy (\x y -> compare (entityTime x) (entityTime y)) ee
        print enemy
        clientV <- peek client
        poke client $ clientV { viewangles = entityOrigin enemy }
        --forM_ ee $ \ent -> clMouseEvent (x (entityOrigin ent)) (y (entityOrigin ent)) (CInt 1)
    !_ <- unlock
    return ()

  -- cvar "r_shownormals" "1"
  --state <- clientActive
  --print state


  -- glDrawElementsCall origDrawEl mode count typ indices

  -- glTexEnvi glTextureEnv glTextureEnvMode glModulate

  -- !_ <- glDisable glDepthTest
  -- !_ <- glEnable glColourMaterial
  glColor3f 1.0 0.2 0.2
  glDrawElementsCall origDrawEl mode count typ indices
  -- !_ <- glEnable glDepthTest
  -- -- glColor3f 1 1 1
  -- glDisable glColourMaterial

  -- drawText "HAAAAX" (10, 10) (0.6, 0.2, 0.2, 0.5)
  -- Bright walls
  -- glTexEnvi glTextureEnv glTextureEnvMode glModulate
  -- glTexEnvi glTextureEnv glTextureEnvMode glDecal
  -- whack
  --when (mode == glTriangles && indices /= nullPtr) $ do
  --  glDepthFunc glAlways
  --  glDrawElementsCall origDrawEl mode count typ indices
  --  glDepthFunc glLEqual
  -- chams
  --when (mode == glQuads || indices == nullPtr) $ glDisable glColourMaterial
  --when (mode /= glQuads && indices /= nullPtr) $ do
  --  glEnable glColourMaterial
  --  glColor3f 0.9 0.2 0
  --  glDrawElementsCall origDrawEl mode count typ indices
  --  glColor3f 1 1 1
  --  glDisable glColourMaterial

  --glDrawElementsCall origDrawEl mode count typ indices

glTexCoordPointer :: CInt -> CInt -> CInt -> Ptr () -> IO ()
glTexCoordPointer sz typ str ptr = do
  orig <- glTexCoordPointerSym
  -- glTexEnvi glTextureEnv glTextureEnvMode glModulate

  glTexCoordPointerCall orig sz typ str ptr
  -- glColor3f 1 1 1

  -- glDepthFunc glLEqual
  -- when (sz == 2 && typ == glFloat && str == 0) $ glDepthFunc glAlways


sdlInitSym :: IO (FunPtr a)
sdlInitSym = do
  dl <- libSdl
  dlsym dl "SDL_Init"

sdlSetRenderTargetSym :: IO (FunPtr a)
sdlSetRenderTargetSym = do
  dl <- libSdl
  dlsym dl "SDL_SetRenderTarget"

glClearSym :: IO (FunPtr a)
glClearSym = do
  dl <- libGl
  dlsym dl "glClear"

glFlushSym :: IO (FunPtr a)
glFlushSym = do
  dl <- libGl
  dlsym dl "glFlush"

glTexCoordPointerSym :: IO (FunPtr a)
glTexCoordPointerSym = do
  dl <- libGl
  dlsym dl "glTexCoordPointer"

sdlInit :: CUInt -> IO CInt
sdlInit flags = do
  origInit <- sdlInitSym
  res <- sdlInitCall origInit flags
  putStrLn $ "SDL Init hook (flags: " ++ show flags ++ ") -> " ++ show res
  return res

sdlSetRenderTarget :: Ptr () -> Ptr () -> IO CInt
sdlSetRenderTarget renderer texture = do
  origRenderTarget <- sdlSetRenderTargetSym
  sdlSetRenderTargetCall origRenderTarget renderer texture

glFlush :: IO ()
glFlush = do
  origFlush <- glFlushSym
  glFlushCall origFlush

glClear :: CInt -> IO ()
glClear mask = do
  origClear <- glClearSym
  glClearCall origClear mask

