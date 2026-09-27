// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {CaptchaProviderType, ICaptchaProvider, VerifyCaptchaParams} from '@pkgs/captcha/src/ICaptchaProvider';
import {createChallenge, randomInt, verifySolution} from 'altcha-lib';
import {deriveKey} from 'altcha-lib/algorithms/pbkdf2';
import type {Challenge} from 'altcha-lib/types';
import {ms} from 'itty-time';
import {z} from 'zod';

export const ALTCHA_ALGORITHM = 'PBKDF2/SHA-256';
const ALTCHA_CHALLENGE_TTL_MS = ms('10 minutes');
const ALTCHA_MAX_TOKEN_LENGTH = 4096;
const HEX_PATTERN = /^[0-9a-f]+$/u;

const AltchaPayloadSchema = z.object({
	challenge: z.object({
		parameters: z.looseObject({
			algorithm: z.literal(ALTCHA_ALGORITHM),
			nonce: z.string().regex(HEX_PATTERN),
			salt: z.string().regex(HEX_PATTERN),
			cost: z.number().int().positive(),
			keyLength: z.number().int().positive(),
			keyPrefix: z.string().regex(HEX_PATTERN),
			keySignature: z.string().regex(HEX_PATTERN),
			expiresAt: z.number().int().positive(),
		}),
		signature: z.string().regex(HEX_PATTERN),
	}),
	solution: z.object({
		counter: z.number().int().min(0),
		derivedKey: z.string().regex(HEX_PATTERN),
		time: z.number().optional(),
	}),
});

type AltchaPayload = z.infer<typeof AltchaPayloadSchema>;

export interface AltchaProviderOptions {
	hmacSignatureSecret: string;
	hmacKeySignatureSecret: string;
	cost: number;
	maxCounter: number;
	claimChallenge: (signature: string, ttlSeconds: number) => Promise<boolean>;
	logger?: LoggerInterface;
	now?: () => number;
}

function decodePayload(token: string): AltchaPayload | null {
	if (token.length > ALTCHA_MAX_TOKEN_LENGTH) return null;
	try {
		const parsed = AltchaPayloadSchema.safeParse(JSON.parse(Buffer.from(token, 'base64').toString('utf8')));
		return parsed.success ? parsed.data : null;
	} catch {
		return null;
	}
}

export class AltchaProvider implements ICaptchaProvider {
	readonly type: CaptchaProviderType = 'altcha';
	private readonly options: AltchaProviderOptions;
	private readonly now: () => number;

	constructor(options: AltchaProviderOptions) {
		this.options = options;
		this.now = options.now ?? Date.now;
	}

	async createChallenge(): Promise<Challenge> {
		const {cost, maxCounter, hmacSignatureSecret, hmacKeySignatureSecret} = this.options;
		return await createChallenge({
			algorithm: ALTCHA_ALGORITHM,
			cost,
			counter: randomInt(maxCounter, Math.ceil(maxCounter / 2)),
			deriveKey,
			expiresAt: new Date(this.now() + ALTCHA_CHALLENGE_TTL_MS),
			hmacSignatureSecret,
			hmacKeySignatureSecret,
		});
	}

	async verify({token}: VerifyCaptchaParams): Promise<boolean> {
		const payload = decodePayload(token);
		if (!payload) return false;
		try {
			const result = await verifySolution({
				challenge: payload.challenge,
				solution: payload.solution,
				deriveKey,
				hmacSignatureSecret: this.options.hmacSignatureSecret,
				hmacKeySignatureSecret: this.options.hmacKeySignatureSecret,
			});
			if (!result.verified) {
				this.options.logger?.warn(
					{expired: result.expired, invalidSignature: result.invalidSignature, invalidSolution: result.invalidSolution},
					'ALTCHA verification failed',
				);
				return false;
			}
		} catch (error) {
			this.options.logger?.error({error}, 'Error verifying ALTCHA payload');
			return false;
		}
		const ttlSeconds = Math.max(1, payload.challenge.parameters.expiresAt - Math.floor(this.now() / 1000));
		return await this.options.claimChallenge(payload.challenge.signature, ttlSeconds);
	}
}
