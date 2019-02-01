import { EntitySchema } from 'typeorm';

import { SignedOrderModel } from '../models/SignedOrderModel';

export const signedOrderEntity = new EntitySchema<SignedOrderModel>({
    name: 'SignedOrder',
    target: SignedOrderModel,
    columns: {
        hash: {
            primary: true,
            type: 'text',
        },
        senderAddress: {
            type: 'text',
        },
        makerAddress: {
            type: 'text',
        },
        takerAddress: {
            type: 'text',
        },
        makerAssetData: {
            type: 'text',
        },
        takerAssetData: {
            type: 'text',
        },
        exchangeAddress: {
            type: 'text',
        },
        feeRecipientAddress: {
            type: 'text',
        },
        expirationTimeSeconds: {
            type: 'int',
        },
        makerFee: {
            type: 'double precision',
        },
        takerFee: {
            type: 'double precision',
        },
        makerAssetAmount: {
            type: 'double precision',
        },
        takerAssetAmount: {
            type: 'double precision',
        },
        salt: {
            type: 'text',
        },
        signature: {
            type: 'text',
        },
    },
});
