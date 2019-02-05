export class SignedOrderModel {
    public hash: string;
    public senderAddress: string;
    public makerAddress: string;
    public takerAddress: string;
    public makerAssetData: string;
    public takerAssetData: string;
    public exchangeAddress: string;
    public feeRecipientAddress: string;
    public expirationTimeSeconds: number;
    public makerFee: string;
    public takerFee: string;
    public makerAssetAmount: string;
    public takerAssetAmount: string;
    public salt: string;
    public signature: string;
    constructor(
        opts: {
            hash: string;
            senderAddress: string;
            makerAddress: string;
            takerAddress: string;
            makerAssetData: string;
            takerAssetData: string;
            exchangeAddress: string;
            feeRecipientAddress: string;
            expirationTimeSeconds: number;
            makerFee: string;
            takerFee: string;
            makerAssetAmount: string;
            takerAssetAmount: string;
            salt: string;
            signature: string;
        } = {
            hash: 'usedForEntityMetadata',
            senderAddress: 'usedForEntityMetadata',
            makerAddress: 'usedForEntityMetadata',
            takerAddress: 'usedForEntityMetadata',
            makerAssetData: 'usedForEntityMetadata',
            takerAssetData: 'usedForEntityMetadata',
            exchangeAddress: 'usedForEntityMetadata',
            feeRecipientAddress: 'usedForEntityMetadata',
            expirationTimeSeconds: 10,
            makerFee: 'usedForEntityMetadata',
            takerFee: 'usedForEntityMetadata',
            makerAssetAmount: 'usedForEntityMetadata',
            takerAssetAmount: 'usedForEntityMetadata',
            salt: 'usedForEntityMetadata',
            signature: 'usedForEntityMetadata',
        },
    ) {
        this.hash = opts.hash;
        this.senderAddress = opts.senderAddress;
        this.makerAddress = opts.makerAddress;
        this.takerAddress = opts.takerAddress;
        this.makerAssetData = opts.makerAssetData;
        this.takerAssetData = opts.takerAssetData;
        this.exchangeAddress = opts.exchangeAddress;
        this.feeRecipientAddress = opts.feeRecipientAddress;
        this.expirationTimeSeconds = opts.expirationTimeSeconds;
        this.makerFee = opts.makerFee;
        this.takerFee = opts.takerFee;
        this.makerAssetAmount = opts.makerAssetAmount;
        this.takerAssetAmount = opts.takerAssetAmount;
        this.salt = opts.salt;
        this.signature = opts.signature;
    }
}
