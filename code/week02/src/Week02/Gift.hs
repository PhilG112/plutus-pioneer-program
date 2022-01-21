{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Week02.Gift where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
import           Schema               (ToSchema)
import           Text.Printf          (printf)
import Data.String
import qualified PlutusTx.Builtins as Builtins
import Data.Void

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# INLINABLE mkValidator #-}
--             Datum          Redeemer       Context
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: Scripts.Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    -- Pay a certain amount to this script address, pass in the validator hash, datum, amount to pay
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx -- Submit the transaction
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx -- wait for confirmation of the transaction
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt scrAddress -- Looks up all utxos sitting at an address
    let orefs   = fst <$> Map.toList utxos --fmap fst (Map.toList utxos), gets all references to the utxos
        lookups = Constraints.unspentOutputs utxos      <> -- lookups to construct the transaction, tell the wallet where to findd all the utxos
                  Constraints.otherScript validator -- Inform the wallet about the validator
        tx :: Constraints.TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer (Builtins.mkI 17) | oref <- orefs] -- Construct list of transactions based on some constraints, pretty simple list comprehension here
    ledgerTx <- submitTxConstraintsWith @Void lookups tx -- submit transaction
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx -- await confirmation
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints -- Offer the user a choice, either give or grab an recursively do it
  where
    give' = endpoint @"give" give -- give endpoint, endpoint will block execution and wait for the user to provide an integer
    grab' = endpoint @"grab" $ const grab -- 

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
