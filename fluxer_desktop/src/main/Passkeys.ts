// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import {getDesktopLocalAppProtocol} from '@electron/main/LocalAppProtocol';
import {httpOriginSource} from '@electron/main/LocalAppRuntimePlans';
import {isLocalAppURL} from '@electron/main/LocalAppURL';
import {isOfficialInstanceHost} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import type {
	create as nativeCreateFn,
	get as nativeGetFn,
	isSupported as nativeIsSupportedFn,
	PublicKeyCredential,
	PublicKeyCredentialCreationOptions,
	PublicKeyCredentialDescriptor,
	PublicKeyCredentialRequestOptions,
} from '@fluxer/webauthn';
import type {
	AuthenticationExtensionsClientOutputs,
	AuthenticationResponseJSON,
	AuthenticatorAssertionResponseJSON,
	AuthenticatorAttestationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialDescriptorJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/browser';
import type {IpcMainInvokeEvent} from 'electron';
import {BrowserWindow, ipcMain} from 'electron';

const logger = createChildLogger('Passkeys');
const requireModule = createRequire(import.meta.url);
const emptyExtensions: AuthenticationExtensionsClientOutputs = {};
const base64UrlToBuffer = (value: string): Buffer => {
	const normalized = value.replace(/-/g, '+').replace(/_/g, '/');
	const padLength = (4 - (normalized.length % 4)) % 4;
	return Buffer.from(`${normalized}${'='.repeat(padLength)}`, 'base64');
};
const bufferToBase64Url = (value: Buffer): string =>
	value.toString('base64').replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');

type NativeCreationOptionsWithoutOrigin = Omit<PublicKeyCredentialCreationOptions, 'origin'>;
type NativeRequestOptionsWithoutOrigin = Omit<PublicKeyCredentialRequestOptions, 'origin'>;

const convertDescriptorList = (
	list?: Array<PublicKeyCredentialDescriptorJSON>,
): Array<PublicKeyCredentialDescriptor> | undefined =>
	list?.map((descriptor) => ({
		id: base64UrlToBuffer(descriptor.id),
		type: 'public-key',
		transports: descriptor.transports,
	}));
const convertRequestOptions = (options: PublicKeyCredentialRequestOptionsJSON): NativeRequestOptionsWithoutOrigin => ({
	...options,
	challenge: base64UrlToBuffer(options.challenge),
	allowCredentials: convertDescriptorList(options.allowCredentials),
});
const convertCreationOptions = (
	options: PublicKeyCredentialCreationOptionsJSON,
): NativeCreationOptionsWithoutOrigin => ({
	attestation: options.attestation,
	authenticatorSelection: options.authenticatorSelection,
	challenge: base64UrlToBuffer(options.challenge),
	excludeCredentials: convertDescriptorList(options.excludeCredentials),
	extensions: options.extensions,
	pubKeyCredParams: options.pubKeyCredParams,
	rp: options.rp,
	timeout: options.timeout,
	user: {...options.user, id: base64UrlToBuffer(options.user.id)},
});

function parseCredentialResponse<T>(credential: PublicKeyCredential): T {
	const payload = credential.response.toString('utf-8');
	if (!payload) {
		throw new Error('Passkey response payload is empty');
	}
	try {
		return JSON.parse(payload) as T;
	} catch (error) {
		throw new Error(
			`Failed to parse passkey response payload: ${error instanceof Error ? error.message : 'unknown error'}`,
		);
	}
}

const normalizeAuthenticatorAttachment = (
	attachment: string | null | undefined,
): AuthenticatorAttachment | undefined => (attachment == null ? undefined : (attachment as AuthenticatorAttachment));
const buildAuthenticationResponse = (credential: PublicKeyCredential): AuthenticationResponseJSON => ({
	id: bufferToBase64Url(credential.rawId),
	rawId: bufferToBase64Url(credential.rawId),
	response: parseCredentialResponse<AuthenticatorAssertionResponseJSON>(credential),
	clientExtensionResults: emptyExtensions,
	type: 'public-key',
	authenticatorAttachment: normalizeAuthenticatorAttachment(credential.authenticatorAttachment),
});
const buildRegistrationResponse = (credential: PublicKeyCredential): RegistrationResponseJSON => ({
	id: bufferToBase64Url(credential.rawId),
	rawId: bufferToBase64Url(credential.rawId),
	response: parseCredentialResponse<AuthenticatorAttestationResponseJSON>(credential),
	clientExtensionResults: emptyExtensions,
	type: 'public-key',
	authenticatorAttachment: normalizeAuthenticatorAttachment(credential.authenticatorAttachment),
});

interface PasskeyCeremonyContext {
	origin?: string;
	windowHandle?: Buffer;
	pin?: string;
}

interface PasskeyRequestContext {
	pin?: string;
	instanceKey?: string;
}

const MAX_PIN_LENGTH = 63;
const MAX_INSTANCE_KEY_LENGTH = 2048;

const sanitizeInstanceKey = (context: PasskeyRequestContext | undefined): string | null => {
	const instanceKey = context?.instanceKey;
	if (typeof instanceKey !== 'string' || instanceKey.length === 0 || instanceKey.length > MAX_INSTANCE_KEY_LENGTH) {
		return null;
	}
	return instanceKey;
};

const sanitizePin = (context: PasskeyRequestContext | undefined): string | undefined => {
	const pin = context?.pin;
	if (typeof pin !== 'string' || pin.length === 0 || Buffer.byteLength(pin, 'utf-8') > MAX_PIN_LENGTH) {
		return undefined;
	}
	return pin;
};

interface PasskeyProvider {
	isSupported: () => Promise<boolean>;
	authenticate: (
		options: PublicKeyCredentialRequestOptionsJSON,
		context: PasskeyCeremonyContext,
	) => Promise<AuthenticationResponseJSON>;
	register: (
		options: PublicKeyCredentialCreationOptionsJSON,
		context: PasskeyCeremonyContext,
	) => Promise<RegistrationResponseJSON>;
}

interface NativeWebAuthnAddon {
	create: typeof nativeCreateFn;
	get: typeof nativeGetFn;
	getBackendInfo?: () => unknown;
	isSupported: typeof nativeIsSupportedFn;
}

class UntrustedPasskeyCeremonyOriginError extends Error {
	public constructor(webAppOrigin: string, apiOrigin: string) {
		super(`Instance web app origin ${webAppOrigin} is not served by its API origin ${apiOrigin}`);
		this.name = 'UntrustedPasskeyCeremonyOriginError';
	}
}

const requireOrigin = (origin: string | undefined): string => {
	if (!origin) {
		throw new Error('Passkey operation requires a browser frame origin.');
	}
	return origin;
};

function loadFluxerWebAuthnAddon(): NativeWebAuthnAddon | null {
	try {
		const addon = requireModule('@fluxer/webauthn') as NativeWebAuthnAddon;
		logger.info('@fluxer/webauthn initialized', addon.getBackendInfo?.());
		return addon;
	} catch (error) {
		logger.warn(`@fluxer/webauthn failed to initialize on ${process.platform}/${process.arch}.`, error);
		return null;
	}
}

function createFailedNativePasskeyProvider(): PasskeyProvider {
	const failed = (): never => {
		throw new Error(`Passkey native backend failed to initialize on ${process.platform}/${process.arch}.`);
	};
	return {
		isSupported: async () => false,
		authenticate: async () => failed(),
		register: async () => failed(),
	};
}

function createNativePasskeyProviderForAddon(addon: NativeWebAuthnAddon, includeOrigin = false): PasskeyProvider {
	return {
		isSupported: addon.isSupported,
		authenticate: async (options, context) => {
			const requestOptions = convertRequestOptions(options);
			const credential = await addon.get({
				...requestOptions,
				origin: includeOrigin ? requireOrigin(context.origin) : '',
				windowHandle: context.windowHandle,
				pin: context.pin,
			});
			return buildAuthenticationResponse(credential);
		},
		register: async (options, context) => {
			const creationOptions = convertCreationOptions(options);
			const credential = await addon.create({
				...creationOptions,
				origin: includeOrigin ? requireOrigin(context.origin) : '',
				windowHandle: context.windowHandle,
				pin: context.pin,
			});
			return buildRegistrationResponse(credential);
		},
	};
}

function createNativePasskeyProvider(): PasskeyProvider {
	const fluxerAddon = loadFluxerWebAuthnAddon();
	if (!fluxerAddon) {
		return createFailedNativePasskeyProvider();
	}
	return createNativePasskeyProviderForAddon(fluxerAddon, true);
}

function createPasskeyProvider(): PasskeyProvider {
	return createNativePasskeyProvider();
}

let passkeyProvider: PasskeyProvider | null = null;

function getPasskeyProvider(): PasskeyProvider {
	passkeyProvider ??= createPasskeyProvider();
	return passkeyProvider;
}

const sharesInstanceSite = (webAppOrigin: string, apiOrigin: string): boolean => {
	if (new URL(webAppOrigin).hostname === new URL(apiOrigin).hostname) {
		return true;
	}
	return isOfficialInstanceHost(webAppOrigin) && isOfficialInstanceHost(apiOrigin);
};

const instanceWebAppOrigin = (instanceKey: string | null): string | null => {
	const protocol = getDesktopLocalAppProtocol();
	const plan = instanceKey == null ? protocol.getActivePlan() : protocol.findPlanForRoute(instanceKey);
	const endpoints = plan?.endpoints;
	if (endpoints == null) {
		return null;
	}
	const webAppOrigin = httpOriginSource(endpoints.webAppEndpoint);
	const apiOrigin = httpOriginSource(endpoints.apiEndpoint);
	if (webAppOrigin == null || apiOrigin == null) {
		return null;
	}
	if (!sharesInstanceSite(webAppOrigin, apiOrigin)) {
		throw new UntrustedPasskeyCeremonyOriginError(webAppOrigin, apiOrigin);
	}
	return webAppOrigin;
};

const eventOrigin = (event: IpcMainInvokeEvent, instanceKey: string | null): string | undefined => {
	const frameUrl = event.senderFrame?.url || event.sender.getURL();
	const origin = isLocalAppURL(frameUrl) ? instanceWebAppOrigin(instanceKey) : httpOriginSource(frameUrl);
	return origin ?? undefined;
};

const eventWindowHandle = (event: IpcMainInvokeEvent): Buffer | undefined => {
	const window = BrowserWindow.fromWebContents(event.sender) ?? BrowserWindow.getFocusedWindow();
	try {
		return window?.getNativeWindowHandle();
	} catch {
		return undefined;
	}
};

const eventCeremonyContext = (
	event: IpcMainInvokeEvent,
	requestContext: PasskeyRequestContext | undefined,
): PasskeyCeremonyContext => ({
	origin: eventOrigin(event, sanitizeInstanceKey(requestContext)),
	windowHandle: eventWindowHandle(event),
	pin: sanitizePin(requestContext),
});

export function registerPasskeyHandlers(): void {
	ipcMain.handle('passkey-is-supported', (): Promise<boolean> => {
		return getPasskeyProvider().isSupported();
	});
	ipcMain.handle(
		'passkey-authenticate',
		async (
			event,
			options: PublicKeyCredentialRequestOptionsJSON,
			requestContext?: PasskeyRequestContext,
		): Promise<AuthenticationResponseJSON> => {
			return getPasskeyProvider().authenticate(options, eventCeremonyContext(event, requestContext));
		},
	);
	ipcMain.handle(
		'passkey-register',
		async (
			event,
			options: PublicKeyCredentialCreationOptionsJSON,
			requestContext?: PasskeyRequestContext,
		): Promise<RegistrationResponseJSON> => {
			return getPasskeyProvider().register(options, eventCeremonyContext(event, requestContext));
		},
	);
}
