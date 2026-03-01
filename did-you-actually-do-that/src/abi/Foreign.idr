||| DYADT Foreign Function Interface Declarations
|||
||| This module declares all C-compatible functions that will be
||| implemented in the Zig FFI layer for DYADT verification.
|||
||| All functions are declared here with type signatures and safety proofs.
||| Implementations live in ffi/zig/
|||
||| SPDX-License-Identifier: PMPL-1.0-or-later

module DYADT.ABI.Foreign

import DYADT.ABI.Types
import DYADT.ABI.Layout

%default total

--------------------------------------------------------------------------------
-- Library Lifecycle
--------------------------------------------------------------------------------

||| Initialize the DYADT verification engine
||| Returns a handle to the engine instance, or Nothing on failure
export
%foreign "C:dyadt_init, libdyadt"
prim__init : PrimIO Bits64

||| Safe wrapper for engine initialization
export
init : IO (Maybe Handle)
init = do
  ptr <- primIO prim__init
  pure (createHandle ptr)

||| Clean up engine resources
export
%foreign "C:dyadt_free, libdyadt"
prim__free : Bits64 -> PrimIO ()

||| Safe wrapper for cleanup
export
free : Handle -> IO ()
free h = primIO (prim__free (handlePtr h))

--------------------------------------------------------------------------------
-- Claim Verification
--------------------------------------------------------------------------------

||| Verify a single claim
||| Takes: handle, claim JSON (UTF-8 pointer), claim length
||| Returns: Result code. Retrieve result via dyadt_get_verdict / dyadt_get_report
export
%foreign "C:dyadt_verify_claim, libdyadt"
prim__verifyClaim : Bits64 -> Bits64 -> Bits32 -> PrimIO Bits32

||| Safe wrapper for claim verification
export
verifyClaim : Handle -> (claimPtr : Bits64) -> (claimLen : Bits32) -> IO (Either Result ())
verifyClaim h ptr len = do
  result <- primIO (prim__verifyClaim (handlePtr h) ptr len)
  pure $ case resultFromInt result of
    Just Ok => Right ()
    Just err => Left err
    Nothing => Left Error

||| Get the verdict from the last verification
||| Returns: Verdict as u32 (use verdictFromInt to decode)
export
%foreign "C:dyadt_get_verdict, libdyadt"
prim__getVerdict : Bits64 -> PrimIO Bits32

||| Safe wrapper for verdict retrieval
export
getVerdict : Handle -> IO (Maybe Verdict)
getVerdict h = do
  v <- primIO (prim__getVerdict (handlePtr h))
  pure (verdictFromInt v)

||| Get the full verification report as JSON string
||| Caller must free the returned string with dyadt_free_string
export
%foreign "C:dyadt_get_report, libdyadt"
prim__getReport : Bits64 -> PrimIO Bits64

||| Safe wrapper for report retrieval
export
getReport : Handle -> IO (Maybe String)
getReport h = do
  ptr <- primIO (prim__getReport (handlePtr h))
  if ptr == 0
    then pure Nothing
    else do
      let str = prim__getString ptr
      primIO (prim__freeString ptr)
      pure (Just str)

--------------------------------------------------------------------------------
-- SLM Ensemble Evaluation
--------------------------------------------------------------------------------

||| Evaluate a claim through the SLM ensemble
||| Takes: handle, evaluation context JSON (UTF-8 pointer), context length
||| Returns: Result code. Retrieve result via dyadt_get_ensemble_result
export
%foreign "C:dyadt_evaluate_slm, libdyadt"
prim__evaluateSlm : Bits64 -> Bits64 -> Bits32 -> PrimIO Bits32

||| Safe wrapper for SLM evaluation
export
evaluateSlm : Handle -> (contextPtr : Bits64) -> (contextLen : Bits32) -> IO (Either Result ())
evaluateSlm h ptr len = do
  result <- primIO (prim__evaluateSlm (handlePtr h) ptr len)
  pure $ case resultFromInt result of
    Just Ok => Right ()
    Just err => Left err
    Nothing => Left Error

||| Get the ensemble result as JSON string
||| Caller must free the returned string with dyadt_free_string
export
%foreign "C:dyadt_get_ensemble_result, libdyadt"
prim__getEnsembleResult : Bits64 -> PrimIO Bits64

||| Safe wrapper for ensemble result retrieval
export
getEnsembleResult : Handle -> IO (Maybe String)
getEnsembleResult h = do
  ptr <- primIO (prim__getEnsembleResult (handlePtr h))
  if ptr == 0
    then pure Nothing
    else do
      let str = prim__getString ptr
      primIO (prim__freeString ptr)
      pure (Just str)

||| Get the suggested verdict from the last SLM evaluation
export
%foreign "C:dyadt_get_suggested_verdict, libdyadt"
prim__getSuggestedVerdict : Bits64 -> PrimIO Bits32

||| Safe wrapper for suggested verdict
export
getSuggestedVerdict : Handle -> IO (Maybe Verdict)
getSuggestedVerdict h = do
  v <- primIO (prim__getSuggestedVerdict (handlePtr h))
  pure (verdictFromInt v)

--------------------------------------------------------------------------------
-- Claim Extraction
--------------------------------------------------------------------------------

||| Extract claims from text
||| Takes: handle, text pointer, text length
||| Returns: Result code. Retrieve claims via dyadt_get_claims
export
%foreign "C:dyadt_extract_claims, libdyadt"
prim__extractClaims : Bits64 -> Bits64 -> Bits32 -> PrimIO Bits32

||| Safe wrapper for claim extraction
export
extractClaims : Handle -> (textPtr : Bits64) -> (textLen : Bits32) -> IO (Either Result ())
extractClaims h ptr len = do
  result <- primIO (prim__extractClaims (handlePtr h) ptr len)
  pure $ case resultFromInt result of
    Just Ok => Right ()
    Just err => Left err
    Nothing => Left Error

||| Get extracted claims as JSON array string
export
%foreign "C:dyadt_get_claims, libdyadt"
prim__getClaims : Bits64 -> PrimIO Bits64

||| Safe wrapper for claim retrieval
export
getClaims : Handle -> IO (Maybe String)
getClaims h = do
  ptr <- primIO (prim__getClaims (handlePtr h))
  if ptr == 0
    then pure Nothing
    else do
      let str = prim__getString ptr
      primIO (prim__freeString ptr)
      pure (Just str)

--------------------------------------------------------------------------------
-- String Operations
--------------------------------------------------------------------------------

||| Convert C string to Idris String
export
%foreign "support:idris2_getString, libidris2_support"
prim__getString : Bits64 -> String

||| Free C string
export
%foreign "C:dyadt_free_string, libdyadt"
prim__freeString : Bits64 -> PrimIO ()

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

||| Get last error message
export
%foreign "C:dyadt_last_error, libdyadt"
prim__lastError : PrimIO Bits64

||| Retrieve last error as string
export
lastError : IO (Maybe String)
lastError = do
  ptr <- primIO prim__lastError
  if ptr == 0
    then pure Nothing
    else pure (Just (prim__getString ptr))

||| Get error description for result code
export
errorDescription : Result -> String
errorDescription Ok = "Success"
errorDescription Error = "Generic error"
errorDescription InvalidParam = "Invalid parameter"
errorDescription OutOfMemory = "Out of memory"
errorDescription NullPointer = "Null pointer"
errorDescription VerificationFailed = "Verification failed"
errorDescription EnsembleTimeout = "Ensemble evaluation timeout"

--------------------------------------------------------------------------------
-- Version Information
--------------------------------------------------------------------------------

||| Get library version
export
%foreign "C:dyadt_version, libdyadt"
prim__version : PrimIO Bits64

||| Get version as string
export
version : IO String
version = do
  ptr <- primIO prim__version
  pure (prim__getString ptr)

||| Get library build info
export
%foreign "C:dyadt_build_info, libdyadt"
prim__buildInfo : PrimIO Bits64

||| Get build information
export
buildInfo : IO String
buildInfo = do
  ptr <- primIO prim__buildInfo
  pure (prim__getString ptr)

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Check if engine is initialized
export
%foreign "C:dyadt_is_initialized, libdyadt"
prim__isInitialized : Bits64 -> PrimIO Bits32

||| Check initialization status
export
isInitialized : Handle -> IO Bool
isInitialized h = do
  result <- primIO (prim__isInitialized (handlePtr h))
  pure (result /= 0)

||| Get the number of SLM backends registered
export
%foreign "C:dyadt_backend_count, libdyadt"
prim__backendCount : Bits64 -> PrimIO Bits32

||| Get backend count
export
backendCount : Handle -> IO Bits32
backendCount h = primIO (prim__backendCount (handlePtr h))

||| Get the number of verification layers active
export
%foreign "C:dyadt_layer_count, libdyadt"
prim__layerCount : Bits64 -> PrimIO Bits32

||| Get layer count
export
layerCount : Handle -> IO Bits32
layerCount h = primIO (prim__layerCount (handlePtr h))
