// SPDX-License-Identifier: AGPL-3.0-or-later

import {server} from '@app/api/test/msw/server';
import {
	checkLiveKitKeys,
	endpointOrigin,
	parseNodeEndpoint,
	probeLiveKit,
	resolveCheckedAddress,
	setVoiceEndpointResolverForTesting,
} from '@app/api/voice/VoiceServerEndpoints';
import {HttpResponse, http} from 'msw';
import {afterEach, describe, expect, it} from 'vitest';

describe('voice server endpoints', () => {
	afterEach(() => setVoiceEndpointResolverForTesting(null));

	it('keeps only the scheme, host and port as the origin', () => {
		expect(endpointOrigin('wss://voice.example.com/livekit-fra')).toBe('wss://voice.example.com');
		expect(endpointOrigin('wss://203.0.113.7:8443/livekit-x')).toBe('wss://203.0.113.7:8443');
		expect(endpointOrigin('https://voice.example.com')).toBeNull();
	});

	it('accepts only wss endpoints without IPv6 literals, credentials or queries', () => {
		expect(parseNodeEndpoint('wss://voice.example.com/livekit-a').hostname).toBe('voice.example.com');
		for (const bad of [
			'ws://a.example.com',
			'wss://[2001:db8::1]/x',
			'wss://u:p@a.example.com',
			'wss://a.example.com/?x=1',
		]) {
			expect(() => parseNodeEndpoint(bad)).toThrow();
		}
	});

	it('refuses a host when any address is private, even after a public one', async () => {
		setVoiceEndpointResolverForTesting(async () => ['1.1.1.1']);
		await expect(resolveCheckedAddress(new URL('https://voice.example.com'), false)).resolves.toBe('1.1.1.1');
		setVoiceEndpointResolverForTesting(async () => ['1.1.1.1', '10.0.0.5']);
		await expect(resolveCheckedAddress(new URL('https://voice.example.com'), false)).rejects.toMatchObject({
			reason: 'private',
		});
		await expect(resolveCheckedAddress(new URL('https://voice.example.com'), true)).resolves.toBe('1.1.1.1');
		await expect(resolveCheckedAddress(new URL('https://127.0.0.1'), false)).rejects.toMatchObject({reason: 'private'});
	});

	it('probes the root of the path prefix and checks the keys against the room service', async () => {
		const seen: Array<string> = [];
		server.use(
			http.get('http://voice.test/livekit-a/', () => {
				seen.push('probe');
				return HttpResponse.text('OK');
			}),
			http.get('http://voice.test/other/', () => HttpResponse.text('down', {status: 502})),
			http.post('http://voice.test/livekit-a/twirp/livekit.RoomService/ListRooms', ({request}) => {
				seen.push('list');
				const auth = request.headers.get('authorization') ?? '';
				return HttpResponse.json({}, {status: auth.startsWith('Bearer ') ? 200 : 401});
			}),
		);
		setVoiceEndpointResolverForTesting(async () => ['1.1.1.1']);
		const options = {checkAddress: true, allowPrivate: false, timeoutMs: 2000};
		await probeLiveKit('ws://voice.test/livekit-a', options);
		await checkLiveKitKeys('ws://voice.test/livekit-a', {apiKey: 'k', apiSecret: 's'.repeat(32)}, options);
		await expect(probeLiveKit('ws://voice.test/other', options)).rejects.toMatchObject({
			reason: 'unreachable',
			message: 'HTTP 502',
		});
		setVoiceEndpointResolverForTesting(async () => ['192.168.1.2']);
		await expect(probeLiveKit('ws://voice.test/livekit-a', options)).rejects.toMatchObject({reason: 'private'});
		expect(seen).toEqual(['probe', 'list']);
	});
});
