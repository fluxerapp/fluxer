// SPDX-License-Identifier: AGPL-3.0-or-later

import {InvalidPhoneNumberError} from '@fluxer/errors/src/domains/auth/InvalidPhoneNumberError';
import {SmsVerificationUnavailableError} from '@fluxer/errors/src/domains/auth/SmsVerificationUnavailableError';
import {createMockLogger} from '@fluxer/logger/src/mock';
import {TwilioSmsProvider, TwilioVerificationRateLimitError} from '@pkgs/sms/src/providers/TwilioSmsProvider';
import {describe, expect, it} from 'vitest';

interface TwilioRequest {
	url: string;
	authHeader: string;
	body: string;
}

interface TwilioLookupRequest {
	url: string;
	method: string | undefined;
	authHeader: string;
}

function getCapturedRequest(request: TwilioRequest | null): TwilioRequest {
	if (!request) {
		throw new Error('Expected Twilio request to be captured');
	}
	return request;
}

function getCapturedLookupRequest(request: TwilioLookupRequest | null): TwilioLookupRequest {
	if (!request) {
		throw new Error('Expected Twilio lookup request to be captured');
	}
	return request;
}

describe('TwilioSmsProvider', () => {
	it('calls Twilio Verify start endpoint with expected payload', async () => {
		let capturedRequest: TwilioRequest | null = null;
		const fetchStub: typeof fetch = async (_input, init) => {
			capturedRequest = {
				url: String(_input),
				authHeader: (init?.headers as Record<string, string>)?.Authorization,
				body: init?.body as string,
			};
			return new Response(JSON.stringify({success: true}), {status: 200});
		};
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: fetchStub,
		});
		const phone = '+15551234567';
		await provider.startVerification(phone);
		const request = getCapturedRequest(capturedRequest);
		expect(request.url).toBe('https://verify.twilio.com/v2/Services/VA123/Verifications');
		expect(request.authHeader).toBe(`Basic ${Buffer.from('AC123:twilio-secret').toString('base64')}`);
		expect(request.body).toContain('To=%2B15551234567');
		expect(request.body).toContain('Channel=sms');
	});
	it('can start verification with auto channel, device IP, and programmable rate limits', async () => {
		let capturedRequest: TwilioRequest | null = null;
		const fetchStub: typeof fetch = async (_input, init) => {
			capturedRequest = {
				url: String(_input),
				authHeader: (init?.headers as Record<string, string>)?.Authorization,
				body: init?.body as string,
			};
			return new Response(JSON.stringify({channel: 'auto'}), {status: 200});
		};
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: fetchStub,
		});
		const result = await provider.startVerificationWithResult('+15551234567', {
			channel: 'auto',
			deviceIp: '203.0.113.10',
			rateLimits: {
				fluxer_user_id: '123',
				fluxer_phone_prefix: '+1555',
			},
		});
		const request = getCapturedRequest(capturedRequest);
		expect(result).toEqual({channel: 'auto'});
		expect(request.body).toContain('Channel=auto');
		expect(request.body).toContain('DeviceIp=203.0.113.10');
		expect(request.body).toContain('RateLimits%5Bfluxer_user_id%5D=123');
		expect(request.body).toContain('RateLimits%5Bfluxer_phone_prefix%5D=%2B1555');
	});
	it('returns true when verification check is approved', async () => {
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: async () => new Response(JSON.stringify({status: 'approved'}), {status: 200}),
		});
		const result = await provider.checkVerification('+15551234567', '123456');
		expect(result).toBe(true);
	});
	it('returns false when verification check is rejected', async () => {
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: async () => new Response(JSON.stringify({status: 'pending'}), {status: 200}),
		});
		expect(await provider.checkVerification('+15551234567', '123456')).toBe(false);
	});
	it('throws InvalidPhoneNumberError for Twilio invalid phone code', async () => {
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: async () =>
				new Response(JSON.stringify({code: 21211, message: 'Invalid To phone number'}), {status: 400}),
		});
		await expect(provider.startVerification('+15550000000')).rejects.toThrow(InvalidPhoneNumberError);
	});
	it('throws TwilioVerificationRateLimitError for Twilio max-send-attempt responses', async () => {
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: async () =>
				new Response(JSON.stringify({code: 60203, message: 'Max send attempts reached'}), {status: 403}),
		});
		const error = await provider.startVerification('+15551234567').catch((err: unknown) => err);
		expect(error).toBeInstanceOf(TwilioVerificationRateLimitError);
		expect(error).toMatchObject({
			message: 'Too many verification texts were sent recently. Try again later.',
			twilioStatus: 403,
			twilioCode: 60203,
			cooldownScope: 'phone',
			cooldownMs: 600000,
		});
	});
	it('throws TwilioVerificationRateLimitError for Fraud Guard blocks', async () => {
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: async () =>
				new Response(JSON.stringify({code: 60410, message: 'Blocked by Verify Fraud Guard'}), {status: 403}),
		});
		const error = await provider.startVerification('+15551234567').catch((err: unknown) => err);
		expect(error).toBeInstanceOf(TwilioVerificationRateLimitError);
		expect(error).toMatchObject({
			message: 'Phone verification is temporarily blocked for this destination. Try again later.',
			twilioStatus: 403,
			twilioCode: 60410,
			cooldownScope: 'phone',
			cooldownMs: 43200000,
		});
	});
	it('throws SmsVerificationUnavailableError for unexpected non-OK start responses', async () => {
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: async () => new Response(JSON.stringify({code: 30001, message: 'Queue overflow'}), {status: 503}),
		});
		const error = await provider.startVerification('+15551234567').catch((err: unknown) => err);
		expect(error).toBeInstanceOf(SmsVerificationUnavailableError);
	});
	it('throws SmsVerificationUnavailableError when start verification request fails before a response', async () => {
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: async () => {
				throw new TypeError('Failed to fetch');
			},
		});
		await expect(provider.startVerification('+15551234567')).rejects.toThrow(SmsVerificationUnavailableError);
	});
	it('throws TwilioVerificationRateLimitError for max verification-check attempts', async () => {
		const provider = new TwilioSmsProvider({
			config: {
				accountSid: 'AC123',
				authToken: 'twilio-secret',
				verifyServiceSid: 'VA123',
			},
			logger: createMockLogger(),
			fetchFn: async () =>
				new Response(JSON.stringify({code: 60202, message: 'Max check attempts reached'}), {status: 429}),
		});
		const error = await provider.checkVerification('+15551234567', '123456').catch((err: unknown) => err);
		expect(error).toBeInstanceOf(TwilioVerificationRateLimitError);
		expect(error).toMatchObject({
			message: 'Too many verification code checks were attempted. Request a new code later.',
			twilioStatus: 429,
			twilioCode: 60202,
			cooldownScope: 'phone',
		});
	});
	describe('lookupPhone', () => {
		const CONFIG = {accountSid: 'AC123', authToken: 'twilio-secret', verifyServiceSid: 'VA123'};
		it('sends a GET to Lookup v2 with Fields=line_type_intelligence and Basic auth', async () => {
			let capturedRequest: TwilioLookupRequest | null = null;
			const provider = new TwilioSmsProvider({
				config: CONFIG,
				logger: createMockLogger(),
				fetchFn: async (input, init) => {
					capturedRequest = {
						url: String(input),
						method: init?.method,
						authHeader: (init?.headers as Record<string, string>)?.Authorization,
					};
					return new Response(
						JSON.stringify({
							valid: true,
							country_code: 'US',
							line_type_intelligence: {type: 'mobile', carrier_name: 'T-Mobile USA'},
						}),
						{status: 200},
					);
				},
			});
			const result = await provider.lookupPhone('+15551234567');
			const request = getCapturedLookupRequest(capturedRequest);
			expect(request.method).toBe('GET');
			expect(request.url).toBe(
				'https://lookups.twilio.com/v2/PhoneNumbers/%2B15551234567?Fields=line_type_intelligence,sms_pumping_risk',
			);
			expect(request.authHeader).toBe(`Basic ${Buffer.from('AC123:twilio-secret').toString('base64')}`);
			expect(result).toEqual({
				valid: true,
				lineType: 'mobile',
				countryCode: 'US',
				carrierName: 'T-Mobile USA',
				smsPumpingRiskScore: null,
			});
		});
		it('returns nonFixedVoip for Twilio virtual-number responses', async () => {
			const provider = new TwilioSmsProvider({
				config: CONFIG,
				logger: createMockLogger(),
				fetchFn: async () =>
					new Response(
						JSON.stringify({
							valid: true,
							country_code: 'NL',
							line_type_intelligence: {type: 'nonFixedVoip', carrier_name: 'Twilio LLC'},
						}),
						{status: 200},
					),
			});
			const result = await provider.lookupPhone('+3197058046509');
			expect(result?.valid).toBe(true);
			expect(result?.lineType).toBe('nonFixedVoip');
		});
		it('returns personal for +31970-style Dutch personal-number responses', async () => {
			const provider = new TwilioSmsProvider({
				config: CONFIG,
				logger: createMockLogger(),
				fetchFn: async () =>
					new Response(
						JSON.stringify({
							valid: true,
							country_code: 'NL',
							line_type_intelligence: {type: 'personal', carrier_name: null},
						}),
						{status: 200},
					),
			});
			const result = await provider.lookupPhone('+31970123456');
			expect(result?.lineType).toBe('personal');
		});
		it('returns valid=false on 404 (phone not found)', async () => {
			const provider = new TwilioSmsProvider({
				config: CONFIG,
				logger: createMockLogger(),
				fetchFn: async () => new Response('', {status: 404}),
			});
			const result = await provider.lookupPhone('+15550000000');
			expect(result).toEqual({
				valid: false,
				lineType: null,
				countryCode: null,
				carrierName: null,
				smsPumpingRiskScore: null,
			});
		});
		it('returns null (fail-closed trigger) on non-OK non-404 responses', async () => {
			const provider = new TwilioSmsProvider({
				config: CONFIG,
				logger: createMockLogger(),
				fetchFn: async () =>
					new Response(JSON.stringify({code: 20003, message: 'Authentication error'}), {status: 401}),
			});
			const result = await provider.lookupPhone('+15551234567');
			expect(result).toBeNull();
		});
		it('returns null on network failure', async () => {
			const provider = new TwilioSmsProvider({
				config: CONFIG,
				logger: createMockLogger(),
				fetchFn: async () => {
					throw new TypeError('Failed to fetch');
				},
			});
			const result = await provider.lookupPhone('+15551234567');
			expect(result).toBeNull();
		});
		it('normalizes unknown Twilio line-type values to "unknown"', async () => {
			const provider = new TwilioSmsProvider({
				config: CONFIG,
				logger: createMockLogger(),
				fetchFn: async () =>
					new Response(
						JSON.stringify({
							valid: true,
							country_code: 'US',
							line_type_intelligence: {type: 'someBrandNewTwilioType', carrier_name: 'Test'},
						}),
						{status: 200},
					),
			});
			const result = await provider.lookupPhone('+15551234567');
			expect(result?.lineType).toBe('unknown');
		});
		it('parses sms_pumping_risk_score when present', async () => {
			const provider = new TwilioSmsProvider({
				config: CONFIG,
				logger: createMockLogger(),
				fetchFn: async () =>
					new Response(
						JSON.stringify({
							valid: true,
							country_code: 'US',
							line_type_intelligence: {type: 'mobile', carrier_name: 'T-Mobile USA'},
							sms_pumping_risk: {sms_pumping_risk_score: 72},
						}),
						{status: 200},
					),
			});
			const result = await provider.lookupPhone('+15551234567');
			expect(result?.smsPumpingRiskScore).toBe(72);
		});
	});
});
