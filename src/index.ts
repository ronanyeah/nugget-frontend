const { Elm } = require("./Main.elm");
import {
  encodeURL,
  findReference,
  createQR,
  FindReferenceError,
} from "@solana/pay";
import {
  Metadata,
  PROGRAM_ID as METADATA_ID,
} from "@metaplex-foundation/mpl-token-metadata";
import BigNumber from "bignumber.js";
import {
  Keypair,
  PublicKey,
  Connection,
  ConfirmedSignatureInfo,
} from "@solana/web3.js";
import {
  getMint,
  getAssociatedTokenAddressSync,
  TokenError,
} from "@solana/spl-token";
import {
  disconnect,
  getBackpackWallet,
  readWallets,
  connectWallet,
} from "./wallets";

// eslint-disable-next-line fp/no-let
let live = false;

interface TxParams {
  recipient: string;
  amount: string;
  splToken: string | null;
  message: string;
  //label: string;
  //memo: string;
}

interface Details {
  img: string;
  name: string;
  address: string;
  symbol: string;
  decimals: number;
}

// @ts-ignore
// eslint-disable-next-line no-undef
const RPC_URL: string = RPC_URL_;

const connection = new Connection(RPC_URL);

const getValue = (k: string): string | null => {
  return localStorage.getItem(k);
};

(async () => {
  const xnft = await (async () => {
    const isXnft = window.top !== window;
    if (isXnft) {
      const wallet = await getBackpackWallet();
      return {
        meta: {
          name: wallet.name,
          icon: wallet.icon,
        },
        address: wallet.publicKey.toString(),
      };
    } else {
      return null;
    }
  })();

  const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: {
      screen: {
        width: window.innerWidth,
        height: window.innerHeight,
      },
      tokens: [],
      language: getValue("language"),
      xnft,
    },
  });

  app.ports.log.subscribe((txt: string) => console.log(txt));

  app.ports.setValue.subscribe(([k, v]: [string, string]) =>
    localStorage.setItem(k, v)
  );

  app.ports.clearWatch.subscribe(() => {
    // eslint-disable-next-line fp/no-mutation
    live = false;
  });

  app.ports.getWallets.subscribe(() => {
    const wallets = readWallets();
    return app.ports.walletsCb.send(wallets);
  });

  app.ports.disconnect.subscribe(async () => {
    await disconnect();
  });

  app.ports.connect.subscribe((name: string) =>
    wrap(
      async () => {
        const wallet = await connectWallet(name);

        return app.ports.connectCb.send(wallet.toString());
      },
      (_e: any) => {
        app.ports.connectCb.send(null);
      }
    )
  );

  app.ports.openLink.subscribe((x: string) =>
    // @ts-ignore
    window.xnft.openWindow(x)
  );

  app.ports.verifyToken.subscribe((address: string) =>
    wrap(
      async () => {
        const res = (() => {
          try {
            return new PublicKey(address);
          } catch (_) {
            return "Invalid public key.";
          }
        })();

        if (typeof res === "string") {
          return app.ports.tokenErr.send(res);
        }

        const details = await fetchDetails(res, connection);

        app.ports.tokenCb.send(details);
      },
      (err: any) => {
        if (err instanceof TokenError) {
          app.ports.tokenErr.send("Not a valid SPL token.");
        } else {
          app.ports.tokenErr.send("Unexpected error.");
        }
      }
    )
  );

  app.ports.getHistory.subscribe(
    (data: { address: string; mint: string | null }) =>
      wrap(
        async () => {
          const owner = new PublicKey(data.address);
          const mint = data.mint
            ? getAssociatedTokenAddressSync(new PublicKey(data.mint), owner)
            : null;
          const xs = await connection.getSignaturesForAddress(
            mint ? mint : owner,
            {
              limit: 100,
            }
          );

          const balance = await (mint
            ? connection
                .getTokenAccountBalance(mint)
                .then((x) => x.value.uiAmount || 0.0)
                .catch(() => 0.0)
            : connection.getBalance(owner).then((x) => x / Math.pow(10, 9)));

          const memoed = xs
            .filter((x) => x.memo && x.memo.includes("ngt-"))
            .map((x) => x.signature);

          if (memoed.length === 0) {
            return app.ports.historyCb.send({
              mintAddr: data.mint,
              history: [],
              balance,
            });
          }
          const txs = await connection.getParsedTransactions(memoed, {
            maxSupportedTransactionVersion: 0,
          });

          const ixs = txs.flatMap((tx) => {
            if (!tx) {
              return [];
            }
            const signer = tx.transaction.message.accountKeys.find(
              (x) => x.signer
            );
            if (!signer) {
              return [];
            }
            if (signer.pubkey.toString() === data.address) {
              // cancel if owner is signer
              // will trigger is user sent tokens to themselves
              // could change this to be cancel if it is a token send, not receive
              // parsed.info.destination/source
              return [];
            }
            return tx.transaction.message.instructions.flatMap((x: any) =>
              data.mint
                ? x.program === "spl-token" && x.parsed.type === "transfer"
                  ? [
                      {
                        sender: x.parsed.info.multisigAuthority,
                        amount: x.parsed.info.amount,
                        signature: tx.transaction.signatures[0],
                        timestamp: tx.blockTime ? tx.blockTime * 1000 : 0,
                      },
                    ]
                  : []
                : x.program === "system" && x.parsed.type === "transfer"
                ? [
                    {
                      sender: x.parsed.info.source,
                      amount: x.parsed.info.lamports.toString(),
                      signature: tx.transaction.signatures[0],
                      timestamp: tx.blockTime ? tx.blockTime * 1000 : 0,
                    },
                  ]
                : []
            );
          });

          const res = await Promise.all(
            ixs.map(async (info) => {
              return {
                amount: info.amount,
                sender: info.sender,
                receiver: data.address,
                mint: data.mint || "SOL",
                signature: info.signature,
                timestamp: info.timestamp,
              };
            })
          );

          return app.ports.historyCb.send({
            mintAddr: data.mint,
            history: res,
            balance,
          });
        },
        (_e: any) => {
          app.ports.historyErr.send(null);
        }
      )
  );

  app.ports.buildTx.subscribe((params: TxParams) =>
    wrap(
      async () => {
        const reference = new Keypair().publicKey;
        const url = encodeURL({
          recipient: new PublicKey(params.recipient),
          amount: new BigNumber(params.amount),
          splToken: params.splToken
            ? new PublicKey(params.splToken)
            : undefined,
          reference: [reference],
          //label: params.label,
          message: params.message,
          memo: "ngt-" + reference.toString().slice(0, 8),
        }).toString();
        const v = await createQR(url);
        const d = await v.getRawData();

        if (d) {
          const dataUrl = await blobToDataURL(d);

          app.ports.buildTxCb.send(dataUrl);

          //console.log("tracking ref:", reference.toString());

          // eslint-disable-next-line fp/no-mutation
          live = true;
          const res = await (() => {
            const run = async (): Promise<ConfirmedSignatureInfo | null> => {
              if (!live) {
                return null;
              }
              try {
                const val = await findReference(connection, reference, {
                  finality: "confirmed",
                });
                return val;
              } catch (error: any) {
                if (error instanceof FindReferenceError) {
                  //console.log("waiting...");
                  await new Promise((resolve) =>
                    setTimeout(() => resolve(true), 1000)
                  );
                  return run();
                } else {
                  console.error(error);
                  throw error;
                }
              }
            };
            return run();
          })();
          // eslint-disable-next-line fp/no-mutation
          live = false;
          if (res) {
            //console.log(res);
            return app.ports.paymentCb.send(res.signature);
          }
        }
      },
      (_: any) => {
        app.ports.buildTxCb.send(null);
      }
    )
  );
})().catch(console.error);

const wrap = (fn1: () => Promise<void>, fn2: (_: any) => void) =>
  //wrap(
  //async () => {
  //
  //},
  //(_: any) => {
  //
  //}
  //)
  fn1().catch((e) => {
    console.error(e);
    fn2(e);
  });

async function blobToDataURL(blob: Blob): Promise<string> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    // eslint-disable-next-line fp/no-mutation
    reader.onloadend = function () {
      resolve(reader.result as string);
    };
    // eslint-disable-next-line fp/no-mutation
    reader.onerror = reject;
    reader.readAsDataURL(blob);
  });
}

const getMetadataPDA = (mintId: PublicKey): PublicKey => {
  const [addr] = PublicKey.findProgramAddressSync(
    [Buffer.from("metadata"), METADATA_ID.toBuffer(), mintId.toBuffer()],
    METADATA_ID
  );

  return addr;
};

const fetchDetails = async (
  mintId: PublicKey,
  connection: Connection
): Promise<Details> => {
  const mintAcct = await getMint(connection, mintId);
  const metadataPDA = getMetadataPDA(mintId);

  const account = await connection.getAccountInfo(metadataPDA);

  const base = {
    img: "/coin.png",
    name: mintId.toString().slice(0, 6),
    address: mintId.toString(),
    decimals: mintAcct.decimals,
    symbol: mintId.toString().slice(0, 4).toUpperCase(),
  };

  if (!account) {
    return base;
  }

  const [metadata] = Metadata.fromAccountInfo(account);

  /* eslint-disable fp/no-mutation, no-control-regex */
  base.name = metadata.data.name.replace(/\x00/g, "");
  base.symbol = metadata.data.symbol.replace(/\x00/g, "");
  /* eslint-enable fp/no-mutation, no-control-regex */

  const data = await (async () => {
    try {
      const res = await fetch(metadata.data.uri, { cache: "no-store" });
      const json = await res.json();
      // eslint-disable-next-line fp/no-mutating-assign
      return Object.assign(base, {
        img: json.image,
      });
    } catch (e) {
      return base;
    }
  })();

  return data;
};
