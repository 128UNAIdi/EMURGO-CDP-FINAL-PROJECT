{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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

minLovelace :: Integer
minLovelace = 1500000

data LoanParams = LoanParams                             
                    { beneficiary :: PaymentPubKeyHash         
                    , tenor    :: POSIXTime
                    , loanValue :: Integer
                    , interest :: Integer
                    , colName :: CurrencySymbol
                    , colToken :: TokenName                
                    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LoanParams
PlutusTx.makeLift ''LoanParams

data LoanAction = MkLend Lend | Default | Lend
    deriving P.Show

PlutusTx.unstableMakeIsData ''LoanAction
PlutusTx.makeLift ''LoanAction

{-# INLINABLE loanParamsValidator #-}
loanParamsValidator :: LoanParams -> () -> () -> ScriptContext -> Bool
loanParamsValidator loanParams () () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                                   traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary loanParams      

    deadlineReached :: Bool
    deadlineReached = contains (to $ tenor loanParams) $ txInfoValidRange info             

data Loaning                                                          
instance Scripts.ValidatorTypes Loaning where
    type instance DatumType Loaning = ()                             
    type instance RedeemerType Loaning = ()                           

typedLoanValidator :: LoanParams -> Scripts.TypedValidator Loaning
typedLoanValidator loanParams = Scripts.mkTypedValidator @Loaning
    ($$(PlutusTx.compile [|| loanParamsValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode loanParams)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: LoanParams ->  Validator
validator = Scripts.validatorScript . typedLoanValidator 

valHash :: LoanParams -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedLoanValidator

scrAddress :: LoanParams -> Ledger.Address
scrAddress = scriptAddress . validator

-- OFF-CHAIN

data BorrowParams = BorrowParams                                       
    { bpLender   :: !PaymentPubKeyHash
    , bpTenor    :: !POSIXTime
    , bpColName  :: !CurrencySymbol
    , bpColToken :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data LendParams = LendParams                                         
    { lpBorrower :: !PaymentPubKeyHash
    , lpColValue :: !Integer
    , lpInterest :: !Integer
    , lpTenor    :: !POSIXTime
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data SettleParams = SettleParams
    { spBorrower :: !PaymentPubKeyHash
    , spTenor :: !POSIXTime
    , spColName :: !CurrencySymbol
    , spColToken :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data DefaultParams = DefaultParams
    { dpLender :: !PaymentPubKeyHash
    , dpTenor :: !POSIXTime
    , dpColName :: !CurrencySymbol
    , dpColToken :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type LoanSchema =
            Endpoint "borrow" BorrowParams
        .\/ Endpoint "lend" LendParams
        .\/ Endpoint "withdraw" PaymentPubKeyHash
        .\/ Endpoint "settle" SettleParams
        .\/ Endpoint "default" DefaultParams

borrow :: AsContractError e => BorrowParams -> Contract w s e ()
borrow bp = do
    let borDat = LoanParams
                { beneficiary = bpLender bp
                , tenor    = bpTenor bp
                , colName = bpColName bp
                , colToken = bpColToken bp
                }
        v = Value.singleton (bpColName bp) (bpColToken bp) 1 <> Ada.lovelaceValueOf minLovelace
        tx  = Constraints.mustPayToTheScript borDat v
    ledgerTx <- submitTxConstraints (typedLoanValidator borDat) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made a loan collateral using token %s to %s with tenor %s"
        (P.show v)
        (P.show $ bpLender bp)
        (P.show $ bpTenor bp)

lend :: AsContractError e => LendParams -> Contract w s e ()
lend lp = do
    let lendDat = LoanParams
                { beneficiary = lpBorrower lp
                , loanValue = lpColValue lp
                , interest = lpInterest lp
                , tenor = lpTenor lp
                }
        tx  = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ lpColValue lp
    ledgerTx <- submitTxConstraints (typedLoanValidator lendDat) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "Lended %d lovelace to %s with deadline %s"
        ((lpColValue lp) + (lpInterest lp))
        (P.show $ lpBorrower lp)
        (P.show $ lpTenor lp)

withdraw :: forall w s e. AsContractError e => PaymentPubKeyHash -> Contract w s e ()
withdraw pkh = do
    if pkh == ownPaymentPubKeyHash
        then do
            let wlp = LoanParams
                         { beneficiary = pkh}
            utxos <- utxosAt $ scrAddress wlp
            if Map.null utxos
                then logInfo @P.String $ "Loan is not deposited yet"
                else do
                    let orefs   = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript (validator wlp)
                        tx :: Constraints.TxConstraints () ()
                        tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
                    ledgerTx <- submitTxConstraintsWith () lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @P.String $ "Loan fund has been withdrawn by borrower."
        else do
            logInfo @P.String $ "Action can only be performed by contracted borrower!"


settle :: forall w s. SettleParams -> Contract w s Text ()
settle SettleParams{..} = do
    let t      = Value.singleton spColName spColToken 1
        r      = Redeemer $ PlutusTx.toBuiltinData Settle
        sep    = spBorrower SettleParams
        tenor  = spTenor SettleParams
        lookups = Constraints.typedValidatorLookups typedLoanValidator <>
                  Constraints.otherScript loanParamsValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustPayToPubKey sep (t <> Ada.lovelaceValueOf minLovelace)  <>
                  Constraints.mustValidateIn (from tenor)                    <>
                  Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "Settled loan %s for collateral (%s, %s)"
        --(P.show LoanParams)
        (P.show bpColName)
        (P.show bpColToken)


loanDefault :: forall w s. DefaultParams -> Contract w s Text ()
loanDefault DefaultParams{..} = do
    let t      = Value.singleton dpColName dpColToken 1
        r      = Redeemer $ PlutusTx.toBuiltinData Default
        dep    = dplender DefaultParams
        tenor  = dpTenor DefaultParams
        lookups = Constraints.typedValidatorLookups typedLoanValidator <>
                  Constraints.otherScript loanParamsValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustPayToPubKey dep (t <> Ada.lovelaceValueOf minLovelace)  <>
                  Constraints.mustValidateIn (from tenor)                    <>
                  Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "Loan %s defualted. Collateral (%s, %s) sent to lender"
        (P.show LoanParams)
        (P.show bpColName)
        (P.show bpColToken)


endpoints :: Contract () LoanSchema Text ()
endpoints = awaitPromise (borrow' `select` lend' `select` withdraw' `select` settle' `select` loanDefault') >> endpoints
  where
    borrow' = endpoint @"borrow" borrow
    lend'   = endpoint @"lend"   lend
    withdraw' = endpoint @"withdraw" withdraw
    settle' = endpoint @"settle" settle
    loanDefault' = endpoint @"default" loanDefault


mkSchemaDefinitions ''LoanSchema


myCol :: KnownCurrency
myCol = KnownCurrency (ValidatorHash "f") "Collateral" (TokenName "C" :| [])


mkKnownCurrencies ['myCol]
