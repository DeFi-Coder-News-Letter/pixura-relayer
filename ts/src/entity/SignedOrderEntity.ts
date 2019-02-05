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
        },
        makerAddress: {
            type: 'text',
            nullable: false,
        },
        takerAddress: {
            type: 'text',
            nullable: false,
        },
        makerAssetData: {
            type: 'text',
            nullable: false,
        },
        takerAssetData: {
            type: 'text',
            nullable: false,
        },
        exchangeAddress: {
            type: 'text',
            nullable: false,
        },
        feeRecipientAddress: {
            type: 'text',
            nullable: false,
        },
        expirationTimeSeconds: {
            type: 'int',
            nullable: false,
        },
        makerFee: {
            type: 'double precision',
            nullable: false,
        },
        takerFee: {
            type: 'double precision',
            nullable: false,
        },
        makerAssetAmount: {
            type: 'double precision',
            nullable: false,
        },
        takerAssetAmount: {
            type: 'double precision',
            nullable: false,
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
