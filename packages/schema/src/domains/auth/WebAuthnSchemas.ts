import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/server';
import {z} from 'zod';

const AuthenticatorTransportSchema = z.enum(['ble', 'cable', 'hybrid', 'internal', 'nfc', 'smart-card', 'usb']);

const WebAuthnCredentialBase = z.looseObject({
	id: z.string(),
	rawId: z.string(),
	type: z.literal('public-key'),
	authenticatorAttachment: z.enum(['cross-platform', 'platform']).optional(),
	clientExtensionResults: z.looseObject({
		appid: z.boolean().optional(),
		credProps: z.looseObject({rk: z.boolean().optional()}).optional(),
		hmacCreateSecret: z.boolean().optional(),
	}),
});

export const WebAuthnAuthenticationResponse = WebAuthnCredentialBase.extend({
	response: z.looseObject({
		clientDataJSON: z.string(),
		authenticatorData: z.string(),
		signature: z.string(),
		userHandle: z.string().optional(),
	}),
}) satisfies z.ZodType<AuthenticationResponseJSON>;

export const WebAuthnRegistrationResponse = WebAuthnCredentialBase.extend({
	response: z.looseObject({
		clientDataJSON: z.string(),
		attestationObject: z.string(),
		authenticatorData: z.string().optional(),
		transports: z.array(AuthenticatorTransportSchema).optional(),
		publicKeyAlgorithm: z.number().int().optional(),
		publicKey: z.string().optional(),
	}),
}) satisfies z.ZodType<RegistrationResponseJSON>;

export const WebAuthnAuthenticationOptions = z.looseObject({
	challenge: z.string(),
	timeout: z.number().optional(),
	rpId: z.string().optional(),
	allowCredentials: z
		.array(
			z.looseObject({
				id: z.string(),
				type: z.literal('public-key'),
				transports: z.array(AuthenticatorTransportSchema).optional(),
			}),
		)
		.optional(),
	userVerification: z.enum(['discouraged', 'preferred', 'required']).optional(),
	hints: z.array(z.enum(['hybrid', 'security-key', 'client-device'])).optional(),
	extensions: z
		.looseObject({
			appid: z.string().optional(),
			credProps: z.boolean().optional(),
			hmacCreateSecret: z.boolean().optional(),
			minPinLength: z.boolean().optional(),
		})
		.optional(),
}) satisfies z.ZodType<PublicKeyCredentialRequestOptionsJSON>;
