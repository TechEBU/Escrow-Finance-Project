{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module ProjectEscrowContract (endpoints, ProjectEscrowContractSchema, CancellRedeem (..)) where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map hiding (empty)
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKey)
import           Plutus.Contract
import           PlutusTx             (toBuiltinData)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints   as Constraints
import           Ledger.Value           as Value
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Show (..), Semigroup (..), String)
import           Text.Printf          (printf)

newtype CancellRedeem = CancellRedeem Bool
    deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype FeeRedeem = FeeRedeem Bool
    deriving (Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''CancellRedeem [('CancellRedeem, 0)]
PlutusTx.makeIsDataIndexed ''FeeRedeem [('FeeRedeem, 0)]

{-# INLINABLE mkValidator #-}
mkValidator :: () -> CancellRedeem -> ScriptContext -> Bool
mkValidator _ (CancellRedeem isValid) _ = isValid

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh


data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed    = ()
    type instance RedeemerType Typed = CancellRedeem


policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

issuerCS :: PubKeyHash -> CurrencySymbol
issuerCS = scriptCurrencySymbol . policy

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @CancellRedeem

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


type ProjectEscrowContractSchema =
            Endpoint "projectfunds" Integer
        .\/ Endpoint "validation" FeeRedeem
        .\/ Endpoint "cancel" CancellRedeem



projectfunds :: AsContractError e => Integer -> Contract w s e ()
projectfunds amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Put %d lovelaces in the Project Escrow Contract" amount
    logInfo @String $ printf "Time Slots Available: 10am-11am, 2pm-3pm, 3pm-4pm, 5pm-6pm"

cancel :: forall w s e. AsContractError e => CancellRedeem -> Contract w s e ()
cancel r = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos     
        lookups = Constraints.unspentOutputs utxos <> Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ toBuiltinData r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "Cancelled Project and redeemed deposit."

validation :: forall w s e. AsContractError e => FeeRedeem -> Contract w s e ()
validation r = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos     
        lookups = Constraints.unspentOutputs utxos <> Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ toBuiltinData r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "Meeting happened deposit transferred to Communities"



projectfunds' :: Promise () ProjectEscrowContractSchema Text ()
projectfunds' = endpoint @"projectfunds" projectfunds

cancel' :: Promise () ProjectEscrowContractSchema Text ()
cancel' = endpoint @"cancel" cancel

validation' :: Promise () ProjectEscrowContractSchema Text ()
validation' = endpoint @"validation" validation

endpoints :: AsContractError e => Contract () ProjectEscrowContractSchema Text e
endpoints = do
    logInfo @String "Waiting for project confirmation or cancel."
    selectList [projectfunds', cancel', validation'] >>  endpoints

-- these functions are used in the simulator
mkSchemaDefinitions ''ProjectEscrowContractSchema
mkKnownCurrencies []
