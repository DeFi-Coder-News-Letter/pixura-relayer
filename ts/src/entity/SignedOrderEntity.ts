import { EntitySchema } from 'typeorm';

import { SignedOrderModel } from '../models/SignedOrderModel';

export const signedOrderEntity = new EntitySchema<SignedOrderModel>({
    name: 'SignedOrder',
    synchronize: false,
    tableName: 'signed_order',
    target: SignedOrderModel,
    columns: {
        hash: {
            primary: true,
            type: 'text',
            nullable: false,
        },
        senderAddress: {
            type: 'text',
            nullable: false,
            name: 'sender_address',
        },
        makerAddress: {
            type: 'text',
            nullable: false,
            name: 'maker_address',
        },
        takerAddress: {
            type: 'text',
            nullable: false,
            name: 'taker_address',
        },
        makerAssetData: {
            type: 'text',
            nullable: false,
            name: 'maker_asset_data',
        },
        takerAssetData: {
            type: 'text',
            nullable: false,
            name: 'taker_asset_data',
        },
        exchangeAddress: {
            type: 'text',
            nullable: false,
            name: 'exchange_address',
        },
        feeRecipientAddress: {
            type: 'text',
            nullable: false,
            name: 'fee_recipient_address',
        },
        expirationTimeSeconds: {
            type: 'int',
            nullable: false,
            name: 'expiration_time_seconds',
        },
        makerFee: {
            type: 'double precision',
            nullable: false,
            name: 'maker_fee',
        },
        takerFee: {
            type: 'double precision',
            nullable: false,
            name: 'taker_fee',
        },
        makerAssetAmount: {
            type: 'double precision',
            nullable: false,
            name: 'maker_asset_amount',
        },
        takerAssetAmount: {
            type: 'double precision',
            nullable: false,
            name: 'taker_asset_amount',
        },
        salt: {
            type: 'text',
            nullable: false,
        },
        signature: {
            type: 'text',
            nullable: false,
        },
    },
});
