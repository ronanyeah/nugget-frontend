import { getWallets } from "@wallet-standard/core";
import {
  SolanaMobileWalletAdapter,
  createDefaultAddressSelector,
  createDefaultAuthorizationResultCache,
  createDefaultWalletNotFoundHandler,
} from "@solana-mobile/wallet-adapter-mobile";
import {
  WalletAdapter,
  WalletAdapterNetwork,
} from "@solana/wallet-adapter-base";
import {
  GlowWalletAdapter,
  SolflareWalletAdapter,
  //LedgerWalletAdapter,
} from "@solana/wallet-adapter-wallets";
import {
  StandardWalletAdapter,
  isWalletAdapterCompatibleWallet,
} from "@solana/wallet-standard-wallet-adapter-base";
import { PublicKey } from "@solana/web3.js";

// eslint-disable-next-line fp/no-let
let activeWallet: string | null = null;
const options: Record<string, WalletAdapter<string>> = {};

const wallets = getWallets();

const authCache = createDefaultAuthorizationResultCache();

export const getWallet = (): WalletAdapter<string> => {
  if (!activeWallet) {
    throw Error("no wallet connected");
  }
  const wallet = options[activeWallet];
  if (!wallet.connected) {
    // eslint-disable-next-line fp/no-mutation
    activeWallet = null;
    throw Error("wallet disconnected");
  }
  return wallet;
};

export const disconnect = async () => {
  const wallet = getWallet();

  if (wallet.connected) {
    await wallet.disconnect();
    if (activeWallet === "Mobile Wallet Adapter") {
      await authCache.clear();
    }
  }

  // eslint-disable-next-line fp/no-mutation
  activeWallet = null;
};

export const connectWallet = async (name: string): Promise<PublicKey> => {
  const wallet = options[name];

  await wallet.connect();

  if (!wallet.publicKey) {
    throw Error("Not connected");
  }

  // eslint-disable-next-line fp/no-mutation
  activeWallet = name;

  return wallet.publicKey;
};

export const readWallets = () => {
  const registered = wallets
    .get()
    .flatMap((newWallet) =>
      isWalletAdapterCompatibleWallet(newWallet)
        ? [new StandardWalletAdapter({ wallet: newWallet })]
        : []
    );

  const ws = [
    ...registered,
    ...(registered.some((w) => w.name === "Glow")
      ? []
      : [new GlowWalletAdapter()]),
    ...(registered.some((w) => w.name === "Solflare")
      ? []
      : [new SolflareWalletAdapter()]),
    //new LedgerWalletAdapter(),
    new SolanaMobileWalletAdapter({
      addressSelector: createDefaultAddressSelector(),
      appIdentity: {
        name: "Nugget Pay",
        uri: "https://nugget.fyi/",
        icon: "apple-touch-icon.png",
      },
      authorizationResultCache: authCache,
      cluster: WalletAdapterNetwork.Mainnet,
      onWalletNotFound: createDefaultWalletNotFoundHandler(),
    }),
  ].flatMap((adapter) => {
    if (
      adapter.readyState === "Installed" ||
      (adapter.readyState === "Loadable" &&
        adapter.name === "Mobile Wallet Adapter")
    ) {
      // eslint-disable-next-line fp/no-mutation
      options[adapter.name] = adapter;
      return {
        name: adapter.name,
        icon: adapter.icon,
      };
    } else {
      return [];
    }
  });
  return ws;
};

export const getBackpackWallet = async (): Promise<WalletAdapter<string>> =>
  new Promise((res) => {
    window.addEventListener("load", () => {
      // @ts-ignore
      window.xnft.on("connect", async () => {
        // @ts-ignore
        const wallet = new StandardWalletAdapter({ wallet: wallets.get()[0] });
        await wallet.connect();
        res(wallet);
      });
    });
  });
